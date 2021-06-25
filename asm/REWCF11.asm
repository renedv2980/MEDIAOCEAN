*          DATA SET REWCF11    AT LEVEL 012 AS OF 05/04/06                      
*PHASE T83A11C                                                                  
         TITLE 'REWCF11  - CONTRACT WORKSHEET DOWNLOAD'                         
*INCLUDE REGENSTC                                                               
***********************************************************************         
*                                                                     *         
*         REWCF11 --- GENERATE CONTRACT ORDER WORKSHEET FOR DOWNLOAD *          
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 21MAR02 (RHV) --- ORIGINATION DATE                                  *         
* 01MAY02 (JRD) --- FIX LOOP IN HIATUS                                *         
* 01MAY02 (SKU) --- FIX DUMP IN SPL                                   *         
*               --- FIX FORGOTTEN DEPENDENT USING                     *         
* 23SEP02 (JRD) --- CHANGE ORDER OF STATUS,REV,COMBO TO MATCH XML     *         
*                   SPEC                                              *         
*                                                                     *         
***********************************************************************         
***********************************************************************         
*  INPUT:                                                                       
*       PARAMETER 1 = A(CONTRACT NUMBER) (PL5)                                  
*                                                                               
*                 2 = A(REWCFWRK)                                               
*                 3 = A(FAMAP)                                                  
*                                                                               
***********************************************************************         
REWCFWKS CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYWRKLQ,REWCF11,CLEAR=YES                                        
         LR    RA,RC                                                            
         USING MYWORKD,RA                                                       
         L     RC,4(R1)                                                         
         USING WORKD,RC                                                         
***      L     R?,8(R1)                                                         
***      USING WCFMAPD,R?                                                       
         LR    R9,RB                                                            
         AHI   R9,COMMON-REWCFWKS                                               
         USING COMMON,R9                                                        
*                                                                               
* READ CONTRACT RECORD                                                          
*                                                                               
         L     RF,0(R1)            A(CON NUM PL5)                               
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),0(5,RF)                                               
         MVO   WORK(5),WORK+10(5) CONTRACT NUM IN 9'S COMPLEMENT                
*                                                                               
         XC    KEY,KEY                                                          
K        USING RCONPTYP,KEY                                                     
         MVI   K.RCONPTYP,X'8C'                                                 
         MVC   K.RCONPREP,REPALPHA                                              
         MVC   K.RCONPCON,WORK                                                  
         DROP  K                                                                
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BNE   EXIT                                                             
         GOTO1 VGETREC,AIO1                                                     
         L     R8,AIO1                                                          
         USING RCONREC,R8                                                       
*                                  SAVE OFF SOME CONTRACT STUFF                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   RDCON050                                                         
         USING RCONSEND,R6                                                      
         MVC   SVVER,RCONSRV                                                    
         CLC   RCONSSV,SVVER                                                    
         BNH   *+10                                                             
         MVC   SVVER,RCONSSV       HIGHEST OF STA/REP VERSION #                 
         DROP  R6                                                               
*                                                                               
RDCON050 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   RDCON060                                                         
         USING RCONXEL,R6                                                       
         MVC   SVCONF,RCONCONF                                                  
         DROP  R6                                                               
*                                                                               
RDCON060 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'                                                     
         BAS   RE,GETEL                                                         
         BNE   RDCON070                                                         
         ZIC   R3,1(R6)            ELEM LEN                                     
         AHI   R3,-2               -OVERHEAD                                    
         SR    R2,R2                                                            
         LA    R1,9                MINI ELEM LEN                                
         DR    R2,R1                                                            
         STC   R3,TWACOMBO         # OF STATIONS                                
*                                                                               
RDCON070 DS    0H                                                               
         MVC   AIOREC,AIO2         USE IO2 FOR OTHER REC I/O                    
*                                                                               
* READ REP RECORD                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RREPREC,R6                                                       
         MVI   RREPKTYP,1                                                       
         MVC   RREPKREP,REPALPHA                                                
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RREPKEY),KEYSAVE                                           
         BNE   RDREPX                                                           
         GOTO1 VGETREC,AIOREC                                                   
         L     R6,AIOREC                                                        
         MVC   TWAREPNM,RREPNAME                                                
*                                                                               
         CLI   RREPPROF+4,C'Y'                                                  
         BNE   *+8                                                              
         OI    TWATIME,X'80'       USE 6A-559 INSTEAD                           
*                                  IF N USE NSI DEFAULT, IF Y USE ARB           
         CLI   RREPPROF+5,C'Y'                                                  
         BNE   *+8                                                              
         OI    TWATIME,X'40'       USE ARB                                      
*                                                                               
         CLI   RREPPROF+6,C'Y'                                                  
         BNE   *+8                                                              
         OI    TWATIME,X'20'       USE ALTERNATE SPL SCREEN                     
*                                                                               
         CLI   RREPPROF+7,C'Y'                                                  
         BNE   *+8                                                              
         OI    TWATIME,X'10'       ALLOW HIST/INV DISPLAY                       
*                                                                               
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   RDREPX              NO PROFILE ELEMENT                           
         USING RREPPGMP,R6                                                      
         ZIC   RF,RREPPGM#         # OF PROGRAM UNITS (LOOP COUNTER)            
         LA    R6,RREPPGM1                                                      
         DROP  R6                                                               
         USING RREPPGM1,R6                                                      
RDREP10  CLI   RREPPGM1,RREPQCNT   CONTRACT?                                    
         BE    RDREP20                                                          
         LA    R6,RREPPGML(R6)                                                  
         BCT   RF,RDREP10                                                       
         B     RDREPX              CONTRACT NOT FOUND. USE DEFAULTS.            
RDREP20  MVC   PROFDATA,RREPPGM1   SAVE PROGRAM PROFILES UNIT                   
RDREPX   DS    0H                                                               
         DROP  R6                                                               
*                                                                               
* READ STATION RECORD                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSTAREC,R6                                                       
         MVI   RSTAKTYP,2                                                       
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RCONKSTA                                                
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BNE   RDSTAX                                                           
         DROP  R6                                                               
         GOTO1 VGETREC,AIOREC                                                   
         L     R6,AIOREC                                                        
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   RDSTAX                                                           
         USING RSTAXXEL,R6                                                      
         MVC   TWASTAOP,RSTAOPTA                                                
         DROP  R6                                                               
RDSTAX   DS    0H                                                               
*                                                                               
* READ ADVERTISER RECORD                                                        
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RADVREC,R6                                                       
         MVI   RADVKTYP,8                                                       
         MVC   RADVKREP,REPALPHA                                                
         MVC   RADVKADV,RCONKADV                                                
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RADVKEY),KEYSAVE                                           
         BNE   RDADVX                                                           
         GOTO1 VGETREC,AIOREC                                                   
         L     R6,AIOREC                                                        
         MVC   TWAADVNM(20),RADVNAME                                            
         DROP  R6                                                               
RDADVX   DS    0H                                                               
*                                                                               
* READ SALESPERSON RECORD                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSALREC,R6                                                       
         MVI   RSALKTYP,6                                                       
         MVC   RSALKREP,REPALPHA                                                
         MVC   RSALKSAL,RCONSAL                                                 
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RSALKEY),KEYSAVE                                           
         BNE   RDSALX                                                           
         GOTO1 VGETREC,AIOREC                                                   
         L     R6,AIOREC                                                        
         MVC   TWASALNM,RSALNAME                                                
         MVC   TWASALTL,RSALTEL                                                 
         MVC   TWASALFX,RSALFAX                                                 
         DROP  R6                                                               
RDSALX   DS    0H                                                               
*                                                                               
* READ OFFICE RECORD X2                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING ROFFREC,R6                                                       
         MVI   ROFFKTYP,4                                                       
         MVC   ROFFKREP,REPALPHA                                                
         MVC   ROFFKOFF,RCONKOFF                                                
         GOTO1 VHIGH                                                            
         CLC   KEY(L'ROFFKEY),KEYSAVE                                           
         BNE   RDOFF100                                                         
         GOTO1 VGETREC,AIOREC                                                   
         L     R6,AIOREC                                                        
         MVC   SVOFFNM,ROFFNAME                                                 
         DROP  R6                                                               
*                                                                               
RDOFF100 DS    0H                  NOW READ OFF2 RECORD                         
         LA    R6,KEY                                                           
         USING ROFF2REC,R6                                                      
         MVC   KEY(L'ROFF2KEY),KEYSAVE                                          
         MVI   ROFF2TYP,ROFF2TYQ                                                
         GOTO1 VHIGH                                                            
         CLC   KEY(L'ROFF2KEY),KEYSAVE                                          
         BNE   RDOFFX                                                           
         DROP  R6                                                               
         GOTO1 VGETREC,AIOREC                                                   
         L     R6,AIOREC                                                        
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   RDOFFX                                                           
         USING ROFF2FXE,R6                                                      
         MVC   TWAOFFFX,ROFF2FAX                                                
RDOFFX   DS    0H                                                               
         DROP  R6                                                               
*                                                                               
* READ PRODUCT RECORD                                                           
*                                                                               
         CLC   RCONPRD,SPACES      FROM K REC?                                  
         BNE   RDPRD020            NO - FETCH PRD REC                           
         MVI   ELCODE,X'05'                                                     
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   RDPRDX                                                           
         USING RCONEXEL,R6                                                      
         MVC   SVPROD,RCONEXPR                                                  
         B     RDPRDX                                                           
         DROP  R6                                                               
*                                                                               
RDPRD020 DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RPRDREC,R6                                                       
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKREP,REPALPHA                                                
         MVC   RPRDKPRD,RCONPRD                                                 
         MVC   RPRDKADV,RCONKADV                                                
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RPRDKEY),KEYSAVE                                           
         BNE   RDPRDX                                                           
         GOTO1 VGETREC,AIOREC                                                   
         L     R6,AIOREC                                                        
         MVC   SVPROD,RPRDNAME                                                  
         DROP  R6                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   RDPRDX                                                           
         USING RPRDNELM,R6                                                      
         MVC   SVPTPCD,RPRDNPNT    SAVE POINT PERSON CODE                       
         DROP  R6                                                               
RDPRDX   DS    0H                                                               
*                                                                               
* READ POINT PERSON RECORD                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
PTP      USING RPTPREC,R6                                                       
         MVI   PTP.RPTPKTYP,X'31'                                               
         MVC   PTP.RPTPKREP,REPALPHA                                            
         MVC   PTP.RPTPKREC,SVPTPCD                                             
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RPTPKEY),KEYSAVE                                           
         BNE   RDPTPX                                                           
         GOTO1 VGETREC,AIOREC                                                   
         L     R6,AIOREC                                                        
         MVC   SVPTPNM,PTP.RPTPNAME                                             
         MVC   SVPTPPH,PTP.RPTPFONE    POINT PERSON PHONE NUMBER                
         DROP  PTP                                                              
*                                                                               
         MVI   ELCODE,X'20'        EMAIL ELEMENT PRESENT?                       
         BAS   RE,GETEL                                                         
         BNE   RDPTPX                                                           
*                                                                               
         CLI   1(R6),2             NO EMAIL?                                    
         BNH   RDPTPX                                                           
*                                                                               
         ZIC   RF,1(R6)            GET LENGTH OF ELEMENT                        
         SH    RF,=H'3'            SUBTRACT 3 FOR MOVE                          
         EX    RF,*+8              MOVE EMAIL BY LENGTH                         
         B     *+10                                                             
         MVC   SVPTPEMX(0),2(R6)                                                
         AHI   RF,1                                                             
         STC   RF,SVPTPEML                                                      
*                                                                               
RDPTPX   DS    0H                                                               
*                                                                               
*                                                                               
* READ DEVELOPMENTAL CONTYPE RECORD                                             
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'18'        DEVELOPMENTAL ELEMENT PRESENT?               
         BAS   RE,GETEL                                                         
         BNE   RDDCTX                                                           
*                                                                               
         USING RCONDVEL,R6                                                      
         CLI   RCONDVCT,0                                                       
         BE    RDDCTX                                                           
*                                                                               
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
DCT      USING RDCTKEY,R5                                                       
         XC    KEY,KEY             GET DEVELOPMENTAL CONTRACT TYPE              
         MVI   DCT.RDCTKTYP,X'3B'                                               
         MVC   DCT.RDCTKCTY,RCONDVCT                                            
         MVC   DCT.RDCTKREP,REPALPHA                                            
         DROP  R6                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RDDCTX                                                           
*                                                                               
         MVC   AIOREC,AIO3                                                      
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R6,AIO3             PRINT DESCRIPTION                            
         USING RDCTREC,R6                                                       
         MVC   SVDVCTYP,DCT.RDCTDESC                                            
         DROP  DCT                                                              
RDDCTX   DS    0H                                                               
*                                                                               
* READ AGENCY RECORD & FORMAT FIELDS                                            
*                                                                               
         GOTO1 =A(GAGY),DMCB,(RC),(RA),RR=Y                                     
         GOTO1 =A(FMT),DMCB,(RC),(RA),RR=Y                                      
*                                                                               
* ADD WORKSHEET ELEMENT                                                         
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,WKSDATA,0                                   
*                                                                               
* CONTRACT NUMBER                                                               
*                                                                               
         GOTO1 (RFCONNUM,VREPFACS),DMCB,(1,RCONKCON),(5,WORK)                   
         GOTO1 AADDDATA,DMCB,AFABLK,WKSCON,WORK,0                               
*                                                                               
* GEN DATE                                                                      
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(1,WORK)                                      
         GOTO1 AADDDATA,DMCB,AFABLK,WKSGDATE,WORK,0                             
*                                                                               
* GEN TIME                                                                      
*                                                                               
         L     R1,ACOMFACS                                                      
         AHI   R1,CGETFACT-COMFACSD                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         AP    FATIME,=P'60000'                                                 
         UNPK  DUB,FATIME                                                       
         L     R1,FATIME                                                        
         SLL   R1,4                                                             
         ST    R1,FATIME                                                        
         UNPK  DUB,FATIME                                                       
         DROP  RF                                                               
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSGTIME,DUB+1,6                            
*                                                                               
* DISPLAY MOD/VERSION                                                           
*                                                                               
         GOTO1 =A(GENDMV),RR=Y                                                  
         L     RF,FULL                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,WKSVSTAT,WORK2,(RF)                         
*                                                                               
* MARK AS REVISION?                                                             
*                                                                               
         TM    PROFILES+CNTREVIB,CNTREVIA   MARK AS 'REVISION'?                 
         BZ    REVX                         NO                                  
         TM    TWASTAOP,X'40'      OVERRIDE W/STATION OPT?                      
         BO    REVX                                                             
         CLI   SVVER,1             VERSION > 1?                                 
         BNH   REVX                NO - SKIP MARKING AS REVISION                
         TM    SVCONF,X'80'        CONFIRMED?                                   
         BZ    REVX                YES - SKIP MARKING AS REVISION               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSREVIS,0,0                                
REVX     DS    0H                                                               
*                                                                               
* MARK AS COMBO?                                                                
*                                                                               
         CLI   TWACOMBO,0                                                       
         BE    COMBOX                                                           
         GOTO1 AADDDATA,DMCB,AFABLK,WKSCOMBO,0,0                                
COMBOX   DS    0H                                                               
*                                                                               
* DISPLAY REP                                                                   
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSREPNM,TWAREPNM,0                         
*                                                                               
* DISPLAY STATION                                                               
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 (RFSTAOUT,VREPFACS),DMCB,RCONKSTA,WORK                           
         GOTO1 AADDDATA,DMCB,AFABLK,WKSSTANM,WORK,0                             
*                                                                               
* DISPLAY SALESPERSON / SALES ASSISTANT / OFFICE                                
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSSALNM,TWASALNM,0                         
*                                                                               
**       TM    PROFILES+CNTSFONB,CNTSFONA PRINT SALESPERSON PHONE ON            
**       BZ    SAL050                     ALL VER??                             
         GOTO1 AADDDATA,DMCB,AFABLK,WKSSALPH,TWASALTL,0                         
*                                                                               
SAL050   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BNE   SAL060                                                           
         USING RCONXXEL,R6                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,WKSSALAS,RCONXAST,0                         
         DROP  R6                                                               
*                                                                               
SAL060   DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSOFFNM,SVOFFNM,0                          
*                                                                               
         XC    WORK,WORK                                                        
         OC    TWASALFX,TWASALFX   SALESPERSON FAX NUMBER                       
         BZ    SAL100                                                           
         MVC   WORK+00(16),=C'SALESPERSON FAX#'                                 
         MVC   WORK+17(L'TWASALFX),TWASALFX                                     
         B     SAL120                                                           
*                                                                               
SAL100   DS    0H                  -OR-                                         
         OC    TWAOFFFX,TWAOFFFX   OFFICE FAX NUMBER                            
         BZ    SAL120                                                           
         MVC   WORK+00(12),=C'OFFICE FAX# '                                     
         MVC   WORK+12(3),TWAOFFFX                                              
         MVI   WORK+15,C'-'                                                     
         MVC   WORK+16(3),TWAOFFFX+3                                            
         MVI   WORK+19,C'-'                                                     
         MVC   WORK+20(4),TWAOFFFX+6                                            
*                                                                               
SAL120   DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSFAX#,WORK,0                              
*                                                                               
* DISPLAY AGENCY                                                                
*                                                                               
         XC    WORK,WORK                                                        
         TM    TWAFMTFL,X'08'      CARE OF AGENCY?                              
         BZ    AGY020              NO                                           
*                                                                               
         LA    RE,WORK                                                          
         MVC   0(L'RCONKADV,RE),RCONKADV                                        
         LA    RE,L'RCONKADV+1(RE)                                              
         MVC   0(4,RE),=C'C/O '                                                 
         LA    RE,4(RE)                                                         
         MVC   0(L'TWAAGNM1,RE),TWAAGNM1  AGENCY NAME FROM SCREEN               
         B     AGY030                                                           
*                                                                               
AGY020   DS    0H                                                               
         MVC   WORK(33),TWAAGNM2   AGENCY NAME FOR CONTRACT                     
*                                                                               
AGY030   DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSAGYNM,WORK,0                             
*                                                                               
***      CLI   SVVER,1                                                          
***      BE    AGY050                                                           
***      TM    PROFILES+CNTAADDB,CNTAADDA   PRINT AGY ADD ON ALL VER??          
***      BZ    AGYX                                                             
*                                                                               
AGY050   DS    0H                  AGENCY PHONE                                 
         OC    TWAAGYPH,TWAAGYPH                                                
         BZ    AGY060                                                           
         MVC   WORK+0(3),TWAAGYPH                                               
         MVI   WORK+3,C'-'                                                      
         MVC   WORK+4(3),TWAAGYPH+3                                             
         MVI   WORK+7,C'-'                                                      
         MVC   WORK+8(4),TWAAGYPH+6                                             
         GOTO1 AADDDATA,DMCB,AFABLK,WKSAGYPH,WORK,0                             
*                                                                               
AGY060   DS    0H                  AGENCY FAX                                   
         OC    TWAAFAX,TWAAFAX                                                  
         BZ    AGY070                                                           
         MVC   WORK+0(3),TWAAFAX                                                
         MVI   WORK+3,C'-'                                                      
         MVC   WORK+4(3),TWAAFAX+3                                              
         MVI   WORK+7,C'-'                                                      
         MVC   WORK+8(4),TWAAFAX+6                                              
         GOTO1 AADDDATA,DMCB,AFABLK,WKSAGYFX,WORK,0                             
*                                                                               
AGY070   DS    0H                  AGENCY ADDRESS LINES                         
         GOTO1 AADDDATA,DMCB,AFABLK,WKSAGYA1,TWAAGAD1,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,WKSAGYA2,TWAAGAD2,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,WKSAGYA3,TWAAGAD3,0                         
AGYX     DS    0H                                                               
*                                                                               
* BUYER, ADVERTISER, PRODUCT                                                    
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSBUYNM,TWABUYER,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,WKSADVNM,TWAADVNM,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,WKSPROD,SVPROD,0                            
*                                                                               
* EI CODES                                                                      
*                                                                               
         XC    WORK,WORK                                                        
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   EICDX                                                            
         USING RCONIEL,R6                                                       
         GOTO1 AADDDATA,DMCB,AFABLK,WKSEIADV,RCONIADV,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,WKSEIPRD,RCONIPRD,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,WKSEIPR2,RCONIPR2,0                         
         MVC   WORK(10),RCONXEST                                                
         OC    WORK(10),SPACES                                                  
         CLC   WORK(10),SPACES                                                  
         BNE   *+10                                                             
         MVC   WORK(4),RCONIEST                                                 
         GOTO1 AADDDATA,DMCB,AFABLK,WKSEIEST,WORK,0                             
         DROP  R6                                                               
EICDX    DS    0H                                                               
*                                                                               
* POINT PERSON                                                                  
*                                                                               
         TM    PROFILES+CNTPTPRB,CNTPTPRA                                       
         BZ    PTPX                IF OFF SKIP POINT PERSON                     
         GOTO1 AADDDATA,DMCB,AFABLK,WKSPTPNM,SVPTPNM,0                          
PTPX     DS    0H                                                               
*                                                                               
* FLIGHT DATES                                                                  
*                                                                               
         GOTO1 VDATCON,DMCB,(3,RCONDATE),(1,WORK)                               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSFLTST,WORK,0                             
         GOTO1 VDATCON,DMCB,(3,RCONDATE+3),(1,WORK)                             
         GOTO1 AADDDATA,DMCB,AFABLK,WKSFLTEN,WORK,0                             
*                                                                               
* REP/STATION ORDER COMMENTS                                                    
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'82'        REP OCM                                      
         BAS   RE,GETEL                                                         
OCM050   DS    0H                                                               
         BNE   OCM100                                                           
*                                                                               
         L     R3,AIO2                                                          
         GOTO1 =V(REGENSTC),DMCB,(2,(R6)),(R3),VDMGR,RCONREC,VGETTXT,  +        
               RR=Y                                                             
         BNZ   OCM080              COMMENT NOT FOUND                            
         CLI   0(R3),0             IF NULL, PRINT FREE FORM COMMENT             
         BE    OCM080                                                           
*                                                                               
OCM070   DS    0H                                                               
         CLI   0(R3),X'FF'         DONE                                         
         BE    OCM090                                                           
         ZIC   R2,0(R3)                                                         
         AHI   R2,-1                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,WKSROCM,1(R3),(R2)                          
         LA    R3,1(R2,R3)         NEXT LINE                                    
         C     R3,AIO3             BOUNDARY CHECK                               
         BH    OCM090              OUT OF BOUNDS                                
         B     OCM070              NEXT LINE                                    
*                                                                               
OCM080   DS    0H                                                               
         ZIC   R2,1(R6)                                                         
         AHI   R2,-2                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,WKSROCM,2(R6),(R2)                          
OCM090   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         B     OCM050                                                           
*                                                                               
OCM100   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'92'        STA OCM                                      
         BAS   RE,GETEL                                                         
OCM150   DS    0H                                                               
         BNE   OCMX                                                             
OCM180   DS    0H                                                               
         ZIC   R2,1(R6)                                                         
         AHI   R2,-2                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,WKSSOCM,2(R6),(R2)                          
OCM190   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         B     OCM150                                                           
*                                                                               
OCMX     DS    0H                                                               
*                                                                               
* CONTYPE/DEV CONTYPE                                                           
*                                                                               
         TM    TWAPRFW,X'40'       CONTYPE RECORD OPTION #2                     
         BZ    CONTY040            PROFILE TO CON TYPE/DEV TYPE                 
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSCTYP,SVCONTYP,0                          
         GOTO1 AADDDATA,DMCB,AFABLK,WKSDCTY,SVDVCTYP,0                          
*                                                                               
CONTY040 DS    0H                  COMMENTS                                     
         L     R2,AIO3                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(2),=C'C='                                                   
         MVC   WORK+2(L'RCTYCMMT),SVCTYCMT                                      
*                                                                               
         GOTO1 =V(REGENSTC),DMCB,(3,WORK),(R2),VDMGR,RCONREC,VGETTXT,  +        
               RR=Y                                                             
         BNZ   CONTYX              COMMENT NOT FOUND, PRINT NOTHING             
         CLI   0(R2),0                                                          
         BE    CONTYX                                                           
*                                                                               
CONTY050 DS    0H                                                               
         CLI   0(R2),X'FF'         IF X'FF', PRINT NOTHING                      
         BE    CONTYX                                                           
         ZIC   R3,0(R2)                                                         
         BCTR  R3,0                                                             
         GOTO1 AADDDATA,DMCB,AFABLK,WKSDCTYC,1(R2),(R3)                         
         LA    R2,1(R3,R2)                                                      
         B     CONTY050                                                         
*                                                                               
CONTYX   DS    0H                                                               
*                                                                               
* SEND DARE INFO                                                                
*                                                                               
         GOTO1 =A(DODARE),RR=Y                                                  
*                                                                               
* SEND CONTRACT COMMENTS                                                        
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'02'        REP CCM                                      
         BAS   RE,GETEL                                                         
CCM050   DS    0H                                                               
         BNE   CCMX                                                             
*                                                                               
         L     R3,AIO2                                                          
         GOTO1 =V(REGENSTC),DMCB,(2,(R6)),(R3),VDMGR,RCONREC,VGETTXT,  +        
               RR=Y                                                             
         BNZ   CCM080              COMMENT NOT FOUND                            
         CLI   0(R3),0             IF NULL, PRINT FREE FORM COMMENT             
         BE    CCM080                                                           
*                                                                               
CCM070   DS    0H                                                               
         CLI   0(R3),X'FF'         DONE                                         
         BE    CCM090                                                           
         ZIC   R2,0(R3)                                                         
         AHI   R2,-1                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,WKSCHCMT,1(R3),(R2)                         
         LA    R3,1(R2,R3)         NEXT LINE                                    
         C     R3,AIO3             BOUNDARY CHECK                               
         BH    CCM090              OUT OF BOUNDS                                
         B     CCM070              NEXT LINE                                    
*                                                                               
CCM080   DS    0H                                                               
         ZIC   R2,1(R6)                                                         
         AHI   R2,-2                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,WKSCHCMT,2(R6),(R2)                         
CCM090   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         B     CCM050                                                           
CCMX     DS    0H                                                               
*                                                                               
* SEND AGENCY COMMENTS                                                          
*                                                                               
         GOTO1 =A(AGYCOM),RR=Y                                                  
*                                                                               
* SEND AGENCY RISK & LIABILITY                                                  
*                                                                               
         GOTO1 =A(AGYRISK),RR=Y                                                 
         GOTO1 =A(AGYLIAB),RR=Y                                                 
*                                                                               
* SEND COVERSHEET                                                               
*                                                                               
         GOTO1 =A(PRTCOV),RR=Y                                                  
*                                                                               
* SEND SPL                                                                      
*                                                                               
         GOTO1 =A(DOSPL),RR=Y                                                   
*                                                                               
* SEND BOP                                                                      
*                                                                               
         GOTO1 =A(DOBOP),RR=Y                                                   
*                                                                               
* SEND SA EMAIL OVERRIDE                                                        
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9E'        SA EMAIL OVERRIDE ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   SAEMLX                                                           
*                                                                               
         ZIC   R2,1(R6)                                                         
         AHI   R2,-2                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,WKSSAEML,2(R6),(R2)                         
*                                                                               
SAEMLX   DS    0H                                                               
*                                                                               
*                                                                               
* SEND POINT PERSON EMAIL                                                       
*                                                                               
*                                                                               
         ZIC   R2,SVPTPEML                                                      
         CHI   R2,0                                                             
         BE    PTPEMX                                                           
         GOTO1 AADDDATA,DMCB,AFABLK,WKSPTPEM,SVPTPEMX,(R2)                      
PTPEMX   DS    0H                                                               
*                                                                               
         GOTO1 =A(MKTL),RR=Y                                                    
*                                                                               
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
COMMON   DS    0H                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    DS    0H                  SET CC LOW & FAMSGNO                         
         CLI   *,X'FF'                                                          
         B     EXIT                                                             
EXITNO   LTR   RB,RB               SET CC NOT EQUAL                             
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
EXIT     DS    0H                  JUST EXIT                                    
         XIT1                                                                   
*                                                                               
         GETEL R6,=Y(RCONELEM-RCONREC),ELCODE                                   
*                                                                               
         LTORG                                                                  
*--------------------------------------------------------------------           
*  FAMAP TABLE FOR THE WORKSHEET OBJECT                                         
*   IT SHOULD BE KEPT IN SYNC WITH THE COMMENTED OUT VERSION                    
*   IN REWCFMAP                                                                 
*                                                                               
*--------------------------------------------------------------------           
*--------------------------------------------------------------------           
* CONTRACT HEADER ORDER WORKSHEET DATA                                          
*--------------------------------------------------------------------           
WKSDATA  DC    AL1(WKSDATAD-*),AL2(WKSDATAQ),AL2(WKSDATAX-WKSDATA)              
         DC    X'80'                                                            
WKSDATAD DS    0AL1                                                             
WKSCON   DC    AL1(FLDLEN),AL2(0001),C'CON# ',AL1(MDTCHQ),AL1(008)              
WKSGDATE DC    AL1(FLDLEN),AL2(0002),C'GDATE',AL1(MDTDTQ),AL1(000)              
WKSGTIME DC    AL1(FLDLEN),AL2(0003),C'GTIME',AL1(MDTCHQ),AL1(000)              
WKSVSTAT DC    AL1(FLDLEN),AL2(0004),C'VER  ',AL1(MDTCHQ),AL1(000)              
WKSREVIS DC    AL1(FLDLEN),AL2(0005),C'REVIS',AL1(MDTMDQ),AL1(000)              
WKSCOMBO DC    AL1(FLDLEN),AL2(0006),C'COMBO',AL1(MDTMDQ),AL1(000)              
WKSSTANM DC    AL1(FLDLEN),AL2(0007),C'STA  ',AL1(MDTCHQ),AL1(007)              
WKSREPNM DC    AL1(FLDLEN),AL2(0008),C'REPNM',AL1(MDTCHQ),AL1(033)              
WKSSALNM DC    AL1(FLDLEN),AL2(0009),C'SALNM',AL1(MDTCHQ),AL1(020)              
WKSSALPH DC    AL1(FLDLEN),AL2(0010),C'SALPH',AL1(MDTCHQ),AL1(012)              
WKSSALAS DC    AL1(FLDLEN),AL2(0011),C'SALAS',AL1(MDTCHQ),AL1(009)              
WKSOFFNM DC    AL1(FLDLEN),AL2(0012),C'OFFNM',AL1(MDTCHQ),AL1(020)              
WKSFAX#  DC    AL1(FLDLEN),AL2(0013),C'FAX# ',AL1(MDTCHQ),AL1(030)              
WKSAGYNM DC    AL1(FLDLEN),AL2(0014),C'AGYNM',AL1(MDTCHQ),AL1(033)              
WKSAGYPH DC    AL1(FLDLEN),AL2(0015),C'AGYPH',AL1(MDTCHQ),AL1(012)              
WKSAGYFX DC    AL1(FLDLEN),AL2(0016),C'AGYFX',AL1(MDTCHQ),AL1(012)              
WKSAGYA1 DC    AL1(FLDLEN),AL2(0017),C'AGYA1',AL1(MDTCHQ),AL1(034)              
WKSAGYA2 DC    AL1(FLDLEN),AL2(0018),C'AGYA2',AL1(MDTCHQ),AL1(036)              
WKSAGYA3 DC    AL1(FLDLEN),AL2(0019),C'AGYA3',AL1(MDTCHQ),AL1(036)              
WKSBUYNM DC    AL1(FLDLEN),AL2(0020),C'BUYER',AL1(MDTCHQ),AL1(020)              
WKSADVNM DC    AL1(FLDLEN),AL2(0021),C'ADVNM',AL1(MDTCHQ),AL1(030)              
WKSPROD  DC    AL1(FLDLEN),AL2(0022),C'PROD ',AL1(MDTCHQ),AL1(020)              
WKSEIADV DC    AL1(FLDLEN),AL2(0023),C'EIADV',AL1(MDTCHQ),AL1(004)              
WKSEIPRD DC    AL1(FLDLEN),AL2(0024),C'EIPRD',AL1(MDTCHQ),AL1(004)              
WKSEIEST DC    AL1(FLDLEN),AL2(0025),C'EIEST',AL1(MDTCHQ),AL1(010)              
WKSEIPR2 DC    AL1(FLDLEN),AL2(0026),C'EIPR2',AL1(MDTCHQ),AL1(004)              
WKSPTPNM DC    AL1(FLDLEN),AL2(0027),C'PTPNM',AL1(MDTCHQ),AL1(020)              
WKSFLTST DC    AL1(FLDLEN),AL2(0028),C'FLTST',AL1(MDTDTQ),AL1(000)              
WKSFLTEN DC    AL1(FLDLEN),AL2(0029),C'FLTEN',AL1(MDTDTQ),AL1(000)              
WKSROCM  DC    AL1(FLDLEN),AL2(0030),C'R-OCM',AL1(MDTCHQ),AL1(000)              
WKSSOCM  DC    AL1(FLDLEN),AL2(0031),C'S-OCM',AL1(MDTCHQ),AL1(000)              
WKSCTYP  DC    AL1(FLDLEN),AL2(0032),C'CTYPE',AL1(MDTCHQ),AL1(020)              
WKSDCTY  DC    AL1(FLDLEN),AL2(0033),C'DCTYP',AL1(MDTCHQ),AL1(020)              
WKSDCTYC DC    AL1(FLDLEN),AL2(0034),C'CTCMT',AL1(MDTCHQ),AL1(000)              
WKSDRLK  DC    AL1(FLDLEN),AL2(0035),C'DRLK#',AL1(MDTCHQ),AL1(008)              
WKSDRBRN DC    AL1(FLDLEN),AL2(0036),C'DRBRN',AL1(MDTMDQ),AL1(000)              
WKSDRVAR DC    AL1(FLDLEN),AL2(0037),C'DRVAR',AL1(MDTMDQ),AL1(000)              
WKSDRVO# DC    AL1(FLDLEN),AL2(0038),C'DRVO#',AL1(MDTCHQ),AL1(008)              
WKSDRVC# DC    AL1(FLDLEN),AL2(0039),C'DRVC#',AL1(MDTCHQ),AL1(008)              
WKSDRTP# DC    AL1(FLDLEN),AL2(0040),C'DRTP#',AL1(MDTCHQ),AL1(008)              
WKSHIADT DC    AL1(FLDLEN),AL2(0041),C'HATUS',AL1(MDTCDQ),AL1(000)              
WKSHIACM DC    AL1(FLDLEN),AL2(0042),C'HATCM',AL1(MDTCHQ),AL1(000)              
WKSCHCMT DC    AL1(FLDLEN),AL2(0043),C'CHCMT',AL1(MDTCHQ),AL1(000)              
WKSAGCMT DC    AL1(FLDLEN),AL2(0044),C'AGCMT',AL1(MDTCHQ),AL1(000)              
WKSAGRSK DC    AL1(FLDLEN),AL2(0045),C'AGRSK',AL1(MDTCHQ),AL1(040)              
WKSAGLIA DC    AL1(FLDLEN),AL2(0046),C'AGLIA',AL1(MDTCHQ),AL1(000)              
WKSCVTXT DC    AL1(FLDLEN),AL2(0047),C'CVTXT',AL1(MDTCHQ),AL1(000)              
WKSCVCTL DC    AL1(FLDLEN),AL2(0048),C'CVCTL',AL1(MDTCHQ),AL1(001)              
WKSSPLFC DC    AL1(FLDLEN),AL2(0049),C'SPLFC',AL1(MDTBIQ),AL1(004)              
WKSSPLBD DC    AL1(FLDLEN),AL2(0050),C'SPLBD',AL1(MDTBIQ),AL1(004)              
WKSSPLNA DC    AL1(FLDLEN),AL2(0051),C'SPLNA',AL1(MDTMDQ),AL1(000)              
WKSSPLTT DC    AL1(FLDLEN),AL2(0052),C'SPLTT',AL1(MDTBIQ),AL1(004)              
WKSSPLST DC    AL1(FLDLEN),AL2(0053),C'SPLST',AL1(MDTCHQ),AL1(007)              
WKSSPL$$ DC    AL1(FLDLEN),AL2(0054),C'SPL$$',AL1(MDTBIQ),AL1(004)              
WKSSPLPC DC    AL1(FLDLEN),AL2(0055),C'SPL%%',AL1(MDTBIQ),AL1(004)              
WKSSPLCM DC    AL1(FLDLEN),AL2(0056),C'SPLCM',AL1(MDTCHQ),AL1(000)              
WKSBRTGS DC    AL1(FLDLEN),AL2(0057),C'RTGS ',AL1(MDTCHQ),AL1(003)              
WKSBBOOK DC    AL1(FLDLEN),AL2(0058),C'BBOOK',AL1(MDTCHQ),AL1(009)              
WKSBDEMO DC    AL1(FLDLEN),AL2(0059),C'BDEMO',AL1(MDTCHQ),AL1(010)              
WKSBMKT  DC    AL1(FLDLEN),AL2(0060),C'BMKT ',AL1(MDTCHQ),AL1(003)              
WKSSAEML DC    AL1(FLDLEN),AL2(0061),C'SAEML',AL1(MDTCHQ),AL1(000)              
WKSPTPEM DC    AL1(FLDLEN),AL2(0062),C'PTPEM',AL1(MDTCHQ),AL1(000)              
*MN                                                                             
WKMKTTOT DC    AL1(FLDLEN),AL2(0063),C'MKTTL',AL1(MDTPKQ),AL1(010)              
*MN                                                                             
         DC    X'00'                                                            
WKSDATAX EQU   *                                                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE RERISKTAB                                                      
*                                                                               
WCFMAPD  DSECT                                                                  
       ++INCLUDE REWCFMAP                                                       
*                                                                               
       ++INCLUDE REWCFWRK                                                       
*                                                                               
       ++INCLUDE RECNTPROF                                                      
       ++INCLUDE REGENOFF2                                                      
       ++INCLUDE REGENAGY2                                                      
       ++INCLUDE REGENDCT                                                       
       ++INCLUDE REGENCOV                                                       
       ++INCLUDE FAFACTS                                                        
*                                                                               
         PRINT ON                                                               
MYWORKD  DSECT                                                                  
PROFDATA DS    0CL10               PROFILE DATA FROM REP REC                    
PROFEQU  DS    X                   1=PROFILES LOADED/0=NOT                      
         DS    X                   UNUSED AT THIS TIME                          
PROFILES DS    XL8                 64 PROFILE BITS (CONTRACT PROG)              
P        DS    CL132               DUMMY PRINTLINE                              
TWASTAOP DS    X                   STATION RECORD ADDITIONAL OPTIONS            
SVVER    DS    CL1                 HIGHER OF STATION OR REP VERSION NO.         
SVCONF   DS    CL1                 X'80'=NOT CONFIRMED                          
TWAFMTFL DS    CL1                 WRKSHT/CON FORMAT FLAGS                      
TWACOMBO DS    XL1       B         # OF COMBO STATIONS IN COMBO CONTRAT         
TWAPRFK  DS    XL1                 K FORMAT PROFILE BITS                        
TWAPRFW  DS    XL1                 WRKSHT FORMAT PROFILE BITS                   
WPCOVFLG DS    XL1                 COVERSHEET FLAGS                             
TWAREPNM DS    CL33                REP NAME                                     
TWASALNM DS    CL20                SALESMAN NAME                                
TWASALTL DS    CL12                SALESMAN'S TELEPHONE                         
SVOFFNM  DS    CL20                OFFICE NAME                                  
TWASALFX DS    CL12                SALESPERSON FAX NUMBER                       
TWAOFFFX DS    CL10                OFFICE FAX NUMBER                            
TWAAGNM1 DS    CL20                AGENCY NAME FOR SCREEN                       
TWAAGNM2 DS    CL33                AGENCY NAME FOR CONTRACT                     
TWAAGYPH DS    CL10                AGENCY PHONE NUMBER                          
TWAAFAX  DS    CL10       N        AGENCY EASYLINK FAX NUMBER                   
TWAAGAD1 DS    CL34                AGENCY ADDRESS LINE 1                        
TWAAGAD2 DS    CL36                AGENCY ADDRESS LINE 2                        
TWAAGAD3 DS    CL36                AGENCY ADDRESS LINE 3                        
TWAAGSTT DS    CL2                 AGENCY STATE                                 
TWAAGZIP DS    CL10                AGENCY ZIP CODE                              
TWABUYER DS    CL20                BUYER NAME FOR WRKSHT/CON                    
TWAADVNM DS    CL30                ADVERTISER NAME                              
SVPROD   DS    CL20                PRODUCT                                      
SVPTPCD  DS    CL3                 POINT PERSON CODE                            
SVPTPNM  DS    CL20                POINT PERSON NAME                            
SVPTPPH  DS    CL20                POINT PERSON PHONE NUMBER                    
SVPTPEML DS    X                   POINT PERSON EMAIL LEGNTH                    
SVPTPEMX DS    CL60                POINT PERSON EMAIL                           
SVCONTYP DS    CL20                CONTRACT TYPE DESCRIPTION                    
SVDVCTYP DS    CL20                DEVELOPMENTAL CONTYPE DESCRIPTION            
TWAARISK DS    CL1       AN        AGENCY CREDIT RISK RATING                    
TWAALIAB DS    CL1       AN        AGENCY LIABLILITY POSITION                   
SVCTYCMT DS    CL8                 CONTYPE STANDARD COMMENT                     
MKTOT    DS    F                   TOTAL DOLLARS IN MARKET                      
EST$$$   DS    F                   CONTRACT ESTIMATE $$ AMT                     
*MN                                                                             
MRKTOTL  DS    PL10                TOTAL MARKET DOLLARS                         
*MN                                                                             
STALST   DS    CL54                6 STATIONS                                   
*                                      5 BYTES - CALL LETTERS                   
*                                      4 BYTES - AMOUNT                         
TWATIME  DS    X         B         READ FROM REP PROFILE                        
*                                  X'80'=USE 6A-559A B'CAST DAY INSTEAD         
*                                  X'40'=USE NSI DEFAULT                        
*                                  X'20'=USE ALTERNATE SPL SCREEN               
*                                  X'10'=ALLOW HIST/INV SCREEN                  
*                                                                               
CMTIO    DS    CL4096              STD CMT I/O AREA                             
MYWRKLQ  EQU   *-MYWORKD                                                        
         EJECT                                                                  
*                                                                               
         CSECT                                                                  
*---------------------------------------------------------------*               
* FORMAT CURRENT MOD/VER FOR DISPLAY                                            
*---------------------------------------------------------------*               
GENDMV   NTR1  BASE=*,LABEL=*                                                   
         LA    R5,WORK2            OUTPUT AREA                                  
*   GET REQUIRED ELEMENTS                                                       
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   GDMV600                                                          
         LR    R7,R6                                                            
         USING RCONSEND,R7                                                      
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   GDMV600         MISSING ELEMENT? EXIT ROUTINE                    
         LR    R4,R6                                                            
         USING RCONXEL,R4                                                       
*                                                                               
*   DISPLAY CONF/UNCF/ORIGINAL CONFRIMED                                        
*                                                                               
GDMV100  TM    RCONCONF,X'80'                                                   
         BZ    GDMV110                                                          
         MVC   0(4,R5),=C'UNCF'                                                 
         LA    R5,4(R5)                                                         
         B     GDMV200                                                          
GDMV110  TM    RCONCONF,X'40'                                                   
         BZ    GDMV600         ERROR                                            
         CLI   RCONMOD,0                                                        
         BNE   GDMV120                                                          
         MVC   0(18,R5),=C'ORIGINAL CONFIRMED'                                  
         LA    R5,18(R5)                                                        
         B     GDMV400                                                          
GDMV120  MVC   0(4,R5),=C'CONF'                                                 
         LA    R5,4(R5)                                                         
*                                                                               
* DISPLAY NEXT MOD# (OR MOD# FOR CONFIRMED CONTRACTS)                           
*                                                                               
GDMV200  DS    0H                                                               
         CLI   RCONMOD,X'FF'                                                    
         BE    GDMV300                                                          
         ZIC   R2,RCONMOD                                                       
         TM    RCONCONF,X'80'                                                   
         BZ    *+8                                                              
         LA    R2,1(R2)                                                         
         LA    R5,1(R5)                                                         
         MVC   0(4,R5),=C'MOD '                                                 
         LA    R5,4(R5)                                                         
         EDIT  (R2),(3,0(R5)),ALIGN=LEFT,ZERO=NOBLANK                           
         AR    R5,R0                                                            
*                                                                               
* DISPLAY CURRENT VERSION                                                       
*                                                                               
GDMV300  DS    0H                                                               
         TM    RCONCONF,X'40'                                                   
         BO    GDMV400                                                          
         CLI   RCONMOD,X'FF'                                                    
         BE    GDMV310                                                          
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
GDMV310  LA    R5,1(R5)                                                         
         MVC   0(2,R5),=C'V.'                                                   
         LA    R5,2(R5)                                                         
*                                                                               
         CLC   RCONSRV,RCONSSV                                                  
         BH    GDMV320                                                          
         ZIC   R2,RCONSSV                                                       
         B     GDMV330                                                          
GDMV320  ZIC   R2,RCONSRV                                                       
GDMV330  EDIT  (R2),(3,0(R5)),ALIGN=LEFT,ZERO=NOBLANK                           
         AR    R5,R0                                                            
         CH    R2,=H'1'                                                         
         BE    GDMV600                                                          
*                                                                               
* DISPLAY LAST VERSION/MOD                                                      
*                                                                               
GDMV400  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         BE    GDMV405                                                          
         TM    RCONCONF,X'20'+X'40'                                             
         BNZ   GDMV600                                                          
         SR    R6,R6                                                            
*                        FIND PREVIOUS VERSION NUMBER                           
GDMV405  SR    R0,R0                                                            
         LA    RF,1                                                             
         LA    R2,1                                                             
         LA    RE,RCONS2V1              FIRST STATION VERSION #                 
GDMV410  LA    R1,3                     LOOP THRU LAST 3 VERSION #'S            
GDMV420  CLM   RF,1,0(RE)               NEXT VS. HIGHEST SO FAR                 
         BNL   GDMV430                                                          
         LR    R2,RF                    OLD HIGHEST IS 2ND HIGHEST              
         IC    RF,0(RE)                 REPLACE W/HIGHEST                       
         B     GDMV440                                                          
GDMV430  CLM   R2,1,0(RE)                                                       
         BNL   GDMV440                                                          
         IC    R2,0(RE)                                                         
GDMV440  LA    RE,1(RE)                                                         
         BCT   R1,GDMV420               LOOP THRU 3 VERSION NUMBERS             
         LTR   R0,R0                    FIRST TIME THRU?                        
         BNZ   GDMV450                                                          
         LA    RE,RCONSRV1              FIRST REP VERSION #                     
         LA    R0,1                     FLIP FLAG                               
         B     GDMV410                                                          
GDMV450  DS    0H                                                               
         LA    R5,1(R5)                                                         
         LTR   R6,R6                                                            
         BZ    GDMV460                                                          
         TM    RCONCONF,X'40'                                                   
         BO    GDMV460                                                          
         USING RMODELEM,R6                                                      
         ZIC   R3,RMODEL1V                                                      
         CR    R2,R3                                                            
         BH    GDMV460                                                          
         CLI   RMODEL1M,0                                                       
         BNE   GDMV455                                                          
         MVC   0(14,R5),=C'(LAST=ORIG CF)'                                      
         LA    R5,14(R5)                                                        
         B     GDMV600                                                          
GDMV455  MVC   0(13,R5),=C'(LAST=CF MOD '                                       
         LA    R5,13(R5)                                                        
         EDIT  RMODEL1M,(4,0(R5)),ALIGN=LEFT,ZERO=NOBLANK,TRAIL=C')'            
         AR    R5,R0                                                            
         B     GDMV600                                                          
GDMV460  MVC   0(7,R5),=C'(LAST=V'                                              
         LA    R5,7(R5)                                                         
         TM    RCONCONF,X'40'                                                   
         BZ    *+6                                                              
         LR    R2,RF                                                            
         EDIT  (R2),(4,0(R5)),ALIGN=LEFT,ZERO=NOBLANK,TRAIL=C')'                
         AR    R5,R0                                                            
*                                                                               
* WRAP UP & LEAVE                                                               
*                                                                               
GDMV600  DS    0H                  RETURN LENGTH IN FULL                        
         LA    R0,WORK2                                                         
         SR    R5,R0                                                            
         ST    R5,FULL                                                          
         DROP  R7                                                               
         XIT1                                                                   
         LTORG                                                                  
*********************************************************************           
*        FMT  ---  ROUTINE TO HANDLE FORMATTING OF WORKSHEET                    
*********************************************************************           
FMT      NMOD1 0,**FMT***                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
*                                                                               
         MVI   TWAFMTFL,0               INITIALIZE FLAGS                        
*                                                                               
         MVC   TWABUYER,RCONBUYR        BUYER NAME                              
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         MVC   KEY+22(2),RCONKREP                                               
         MVC   KEY+24(3),RCONSAL                                                
         XC    DUB,DUB                                                          
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFGETREC,VREPFACS),DMCB,KEY,AIO3,0,DUB                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO3                                                          
         USING RSALREC,R6                                                       
         MVC   TWASALNM(20),RSALNAME    SALESMAN NAME                           
         DROP  R6                                                               
*                                                                               
*  SPECIAL CASE - KATZ CONVERTED CONTRACT - AGY NAME & ADDR IN CONREC           
*                                                                               
         TM    RCONMODR+1,X'10'    KATZ CONVERTED CONTRACT?                     
         BNO   FMT080                                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'70'        AGENCY NAME ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   FMT050              NOT FOUND - EXIT                             
         XC    TWAAGNM2,TWAAGNM2   SET A(PRINT FIELD)                           
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   TWAAGNM2(0),2(R6)                                                
FMT050   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'71'        FIRST ADDRESS ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   FMT060              NOT FOUND - EXIT                             
         XC    TWAAGAD1,TWAAGAD1                                                
         XC    TWAAGAD2,TWAAGAD2                                                
         XC    TWAAGAD3,TWAAGAD3                                                
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   TWAAGAD1(0),2(R6)                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'72'        SECOND ADDRESS ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   FMT060              NOT FOUND - EXIT                             
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   TWAAGAD2(0),2(R6)                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'73'        MAY BE A THIRD ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   FMT060              NOT FOUND - EXIT                             
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   TWAAGAD3(0),2(R6)                                                
FMT060   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'6F'        ADVERTISER NAME ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   FMT070              NOT FOUND - EXIT                             
         XC    TWAADVNM,TWAADVNM                                                
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   TWAADVNM(0),2(R6)                                                
FMT070   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'74'        SALESPERSON NAME ELEMENT                     
         BAS   RE,GETEL            (PUT INTO POINT PERSON)                      
         BNE   FMT080              NOT FOUND - EXIT                             
         XC    SVPTPNM,SVPTPNM                                                  
         XC    SVPTPPH,SVPTPPH                                                  
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   SVPTPNM(0),2(R6)                                                 
FMT075   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'75'        SALESPERSON PHONE# ELEMENT                   
         BAS   RE,GETEL            (PUT INTO POINT PERSON PHONE)                
         BNE   FMT078              NOT FOUND - EXIT                             
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   SVPTPPH(0),2(R6)                                                 
FMT078   DS    0H                       SPECIAL CASE - FOR CONVERTED            
         CLI   RCONTYPE,C'N'            TYPE N CONTRACTS, ALWAYS REPL           
         BNE   FMT080                   SAL NAME & PHONE                        
         MVC   TWASALNM,SVPTPNM         REPL SAL NAME W/PT PERS NAME            
         MVC   TWASALTL,SVPTPPH         REPL SAL PH# W/PT PERS PHONE            
         XC    TWASALFX,TWASALFX                                                
         XC    TWAOFFFX,TWAOFFFX                                                
FMT080   DS    0H                                                               
*                                                                               
*  LOOKUP CONTYPE RECORD TO GET FORMAT INFO                                     
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
CTY      USING RCTYREC,R6                                                       
         MVI   CTY.RCTYKTYP,RCTYKTYQ     REC TYPE                               
         MVC   CTY.RCTYKREP,RCONKREP     REP CODE                               
         MVC   CTY.RCTYKCTY,RCONTYPE     CON TYPE                               
         MVC   AIOREC,AIO3                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   FMT650                                                           
         GOTO1 VGETREC,AIOREC                                                   
         L     R6,AIO3                                                          
         MVC   SVCONTYP,CTY.RCTYDESC                                            
         CLI   RCTY1LEN,RCTYELMX                                                
         BL    *+10                OLD RECORD DO NOT HAS S/C CODE               
         MVC   SVCTYCMT,CTY.RCTYCMMT                                            
         DROP  CTY                                                              
*                                                                               
         MVI   ELCODE,X'10'             FORMAT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   FMT700                                                           
         LR    R4,R6                                                            
         USING RCTYFEL,R4                                                       
         MVC   TWAPRFK,RCTYFPRC         SAVE OFF PROFILE BYTES                  
         MVC   TWAPRFW,RCTYFPRW                                                 
*                                                                               
         TM    RCTYFPRA,X'08'           CARE OF AGENCY OVERRIDE?                
         BNZ   FMT180                   YES                                     
*                                                                               
         BAS   RE,COAGY                 CHECK FOR CARE OF AGENCY                
         BNE   FMT180                   NOT A CARE OF AGENCY                    
*                                                                               
         OI    TWAFMTFL,X'08'           SET C/O AGENCY FLAG OVERRIDE            
         B     FMT260                   SKIP CONTYPE AGENCY OVERRIDES           
*                                                                               
FMT180   DS    0H                                                               
         TM    RCTYFA1S,X'40'           REPLACE AGY ADDRESS 1?                  
         BNO   FMT200                   NO - NEXT FIELD                         
         XC    TWAAGAD1,TWAAGAD1                                                
         MVI   HALF,C'G'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT200                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAAGAD1(0),3(R6)                                                
FMT200   DS    0H                                                               
         TM    RCTYFA2S,X'40'           REPLACE AGY ADDRESS 2?                  
         BNO   FMT230                   NO - NEXT FIELD                         
         XC    TWAAGAD2,TWAAGAD2                                                
         MVI   HALF,C'H'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT230                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAAGAD2(0),3(R6)                                                
FMT230   DS    0H                                                               
         TM    RCTYFA3S,X'40'           REPLACE AGY ADDRESS 3?                  
         BNO   FMT240                   NO - NEXT FIELD                         
         XC    TWAAGAD3,TWAAGAD3                                                
         MVI   HALF,C'I'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT240                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAAGAD3(0),3(R6)                                                
FMT240   DS    0H                                                               
         TM    RCTYFANS,X'40'           REPLACE AGY NAME?                       
         BNO   FMT250                   NO - NEXT FIELD                         
         XC    TWAAGNM2,TWAAGNM2                                                
         MVI   HALF,C'E'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT250                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAAGNM2(0),3(R6)                                                
FMT250   DS    0H                                                               
         TM    RCTYFABS,X'40'           REPLACE BUYER NAME?                     
         BNO   FMT260                   NO - NEXT FIELD                         
         XC    TWABUYER,TWABUYER                                                
         MVI   HALF,C'F'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT260                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWABUYER(0),3(R6)                                                
*                                                                               
FMT260   DS    0H                                                               
         DROP  R4                                                               
*                                                                               
*  HANDLE FORMAT OPTION BITS HERE                                               
*                                                                               
         TM    TWAPRFW,X'80'            OPTION #1                               
         BNO   FMT650                                                           
         MVC   TWASALNM,SVPTPNM         REPL SAL NAME W/PT PERS NAME            
         MVC   TWASALTL,SVPTPPH         REPL SAL PH# W/PT PERS PHONE            
         XC    TWASALFX,TWASALFX                                                
         XC    TWAOFFFX,TWAOFFFX                                                
FMT650   DS    0H                       OPTION #2 X'40' IS HANDLED              
         B     FMT702                   ELSEWHERE IN THIS MODULE                
*                                                                               
FMT700   DS    0H                                                               
         BAS   RE,COAGY                 CHECK FOR CARE OF AGENCY                
         BNE   FMT702                   NOT A CARE OF AGENCY                    
*                                                                               
         OI    TWAFMTFL,X'08'           SET C/O AGENCY FLAG OVERRIDE            
*                                                                               
FMT702   DS    0H                                                               
         BAS   RE,REPLADDR              REPLACE AGY ADDRESS?                    
                                                                                
FMTYES   CR    RB,RB                                                            
         B     FMTX                                                             
FMTNO    DS    0H                                                               
         LTR   RB,RB                                                            
FMTX     DS    0H                                                               
         XIT1                                                                   
*----------------------------                                                   
TXTSEEK  DS    0H                                                               
         LR    R0,RE                                                            
         MVI   ELCODE,X'12'                                                     
         L     R6,AIO3                                                          
         BAS   RE,GETEL                                                         
TS010    BNE   TSNO                                                             
         CLC   2(1,R6),HALF                                                     
         BE    TSYES                                                            
         BAS   RE,NEXTEL                                                        
         B     TS010                                                            
*                                                                               
TSNO     SR    R1,R1                                                            
         CR    R1,RB                                                            
         B     *+6                                                              
TSYES    CR    R1,R1                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*----------------------------                                                   
* CHECK ADV REC FOR REPLACEMENT AGY ADDRESS                                     
*----------------------------                                                   
REPLADDR NTR1                                                                   
         XC    KEY,KEY                                                          
K        USING RADVKEY,KEY                                                      
         MVI   K.RADVKTYP,X'08'                                                 
         MVC   K.RADVKADV,RCONKADV                                              
         MVC   K.RADVKREP,RCONKREP                                              
         DROP  K                                                                
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTOX (RFGETREC,VREPFACS),DMCB,KEY,AIO3,0,DUB                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FULL(1),RCONTYPE                                                 
         MVC   FULL+1(2),RCONKOFF                                               
         GOTO1 VHELLO,DMCB,(C'G',=C'REPFIL'),(X'20',AIO3),(3,FULL),0            
         CLI   12(R1),0            GOT IT?                                      
         BNE   FMTYES              NO ADDRESS                                   
*                                                                               
         ZICM  R6,13(R1),3         ELEMENT                                      
R        USING RADVAGEL,R6                                                      
         XC    TWAAGAD1,TWAAGAD1                                                
         XC    TWAAGAD2,TWAAGAD2                                                
         XC    TWAAGAD3,TWAAGAD3                                                
         MVC   TWAAGAD1(34),R.RADVAGA1                                          
         MVC   TWAAGAD2(34),R.RADVAGA2                                          
         MVC   TWAAGAD3(36),R.RADVAGA3                                          
         B     FMTYES                                                           
         DROP  R                                                                
*                                                                               
*----------------------------                                                   
* ROUTINE READS AGENCY RECORD AND RETURNS CC EQUAL IF THE IN CARE OF            
*  FLAG IS ON                                                                   
*                                                                               
*  ROUTINE GETS AN AIO AREA IN WORKING STORAGE AND POINTS R6 AT IT              
*----------------------------                                                   
COAGY    NTR1  WORK=(R6,2000/8)                                                 
         XC    KEY,KEY                                                          
K        USING RAGYKEY,KEY                                                      
         MVI   K.RAGYKTYP,X'0A'                                                 
         MVC   K.RAGYKAGY,RCONKAGY                                              
         MVC   K.RAGYKAOF,RCONKAOF                                              
         MVC   K.RAGYKREP,RCONKREP                                              
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RAGYKEY),KEYSAVE                                           
         BNE   FMTNO                                                            
         GOTO1 VGETREC,DMCB,(R6)                                                
*                                                                               
R        USING RAGYREC,R6                                                       
         TM    R.RAGYFLAG,X'20'    CARE OF AGENCY?                              
         BZ    FMTNO               NO                                           
         B     FMTYES              YES                                          
         DROP  R                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*********************************************************************           
*        GAGY --- GET AGENCY NAME AND ADDRESS                                   
*********************************************************************           
GAGY     NMOD1 0,**GAGY**                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
*                                                                               
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   0(R5),X'0A'                                                      
         MVC   19(4,R5),RCONKAGY     AGENCY                                     
         MVC   23(2,R5),RCONKAOF     OFFICE                                     
         MVC   25(2,R5),REPALPHA     REPALPHA                                   
         MVC   AIOREC,AIO3                                                      
*                                                                               
GAGY10   GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GAGY20                                                           
*        CLC   KEY(25),KEYSAVE                                                  
*        BE    GAGY15                                                           
         DC    H'0'                                                             
*AGY15   MVC   KEY+25(2),=C'ZZ'                                                 
*        XC    KEY+27(5),KEY+27                                                 
*        B     GAGY10                                                           
*                                                                               
GAGY20   DS    0H                                                               
         GOTO1 VGETREC,AIOREC                                                   
         L     R6,AIO3                                                          
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING RAGYELEM,R6                                                      
         MVC   TWAARISK,RAGYRISK                                                
         MVC   TWAALIAB,RAGYLIAB                                                
         MVC   TWAAGNM1,RAGYNAM1   AGENCY NAME FOR SCREEN (20)                  
         MVC   TWAAGNM2,RAGYNAM2   AGENCY NAME FOR CONTRACTS (33)               
         XC    TWAAGAD1,TWAAGAD1                                                
         XC    TWAAGAD2,TWAAGAD2                                                
         XC    TWAAGAD3,TWAAGAD3                                                
         MVC   TWAAGAD1(20),RAGYADD1   ADDRESS LINE 1                           
         MVC   TWAAGAD3(20),RAGYCITY   CITY                                     
         MVC   TWAAGSTT,RAGYSTAT   STATE                                        
         MVC   TWAAGZIP,RAGYZIP    ZIP                                          
*                                                                               
         TM    RAGYFLAG,X'80'           EXPANDED ADDRESS                        
         BZ    GAGY50                                                           
*                                                                               
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   0(R5),X'1A'                                                      
         MVC   19(4,R5),RCONKAGY     AGENCY                                     
         MVC   23(2,R5),RCONKAOF     OFFICE                                     
         MVC   25(2,R5),REPALPHA     REPALPHA                                   
         DROP  R6                                                               
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GAGY50                                                           
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   GAGY50                                                           
         MVC   TWAAGAD1,02(R6)     ADDRESS LINE 1                               
         MVC   TWAAGAD2(34),36(R6) ADDRESS LINE 2                               
*                                                                               
* NOTE: THE NEXT 3 LINES ARE TO FIX THE PROBLEM OF THE CITY BEING               
*       DUPLICATED IN THE 2ND ADDRESS FIELD IN PETRY CONVERTED RECS             
         CLC   TWAAGAD3(20),TWAAGAD2                                            
         BNE   *+10                                                             
         XC    TWAAGAD3,TWAAGAD3                                                
*                                                                               
GAGY50   DS    0H                                                               
*                                                                               
*          FLOAT STATE AND ZIP                                                  
         LA    R1,TWAAGAD3                                                      
         LA    R2,L'TWAAGAD3-1(R1)                                              
GAGY60   CR    R2,R1                                                            
         BL    GAGY70                                                           
         OI    0(R2),X'40'                                                      
         CLI   0(R2),X'40'                                                      
         BNE   GAGY70                                                           
         BCT   R2,GAGY60                                                        
GAGY70   MVC   3(2,R2),TWAAGSTT                                                 
         MVC   7(10,R2),TWAAGZIP                                                
*                                                                               
         OC    TWAAGAD2,SPACES                                                  
         CLC   TWAAGAD2,SPACES                                                  
         BNE   GAGY80                                                           
         MVC   TWAAGAD2,TWAAGAD3                                                
         XC    TWAAGAD3,TWAAGAD3                                                
*                                                                               
GAGY80   DS    0H                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   GAGY82                                                           
*                                                                               
         USING RAGY2FXE,R6                                                      
         MVC   TWAAGYPH,RAGY2FON                                                
         MVC   TWAAFAX,RAGY2FAX                                                 
         DROP  R6                                                               
*                                                                               
GAGY82   DS    0H                                                               
         XMOD1                                                                  
         LTORG                                                                  
*                                                                               
* PRINT DARE INFO                                                               
*                                                                               
DODARE   NTR1  BASE=*,LABEL=*                                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        IS DARE AGY ORDER ELEM THERE?                
         BAS   RE,GETEL                                                         
         BNE   CMT67               NO, CONTINUE                                 
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'+X'01' YES, IS CONTR LINKED TO AGY ORDER?          
         BZ    CMT67               OR TAKEOVER??                                
*                                                                               
         GOTO1 VHEXOUT,DMCB,RCONDRLK,WORK,4                                     
         GOTO1 AADDDATA,DMCB,AFABLK,WKSDRLK,WORK,0  DARE AGY #                  
*                                                                               
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    CMT66                                                            
         TM    RCONDRF2,X'80'                                                   
         BZ    CMT65A                                                           
         GOTO1 AADDDATA,DMCB,AFABLK,WKSDRBRN,0,0  MARK BRAND ORDER              
         B     CMT66                                                            
*                                                                               
CMT65A   DS    0H                                                               
         TM    RCONDRF2,X'40'                                                   
         BZ    CMT66                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,WKSDRVAR,0,0  MARK VARIOUS ORDER            
*                                                                               
         GOTO1 VHEXOUT,DMCB,RCONDRVN,WORK,4                                     
         GOTO1 AADDDATA,DMCB,AFABLK,WKSDRVO#,WORK,0  VARIOUS ORD #              
         GOTO1 VHEXOUT,DMCB,RCONDRCN,WORK,4                                     
         GOTO1 AADDDATA,DMCB,AFABLK,WKSDRVC#,WORK,0  VARIOUS CON #              
         DROP  R6                                                               
*                                                                               
CMT66    DS    0H                                                               
         BAS   RE,HIATUS           PRINT DARE HIATUS INFO                       
         B     CMT70                                                            
*                                                                               
CMT67    DS    0H                                                               
         BAS   RE,NODAHIAT         PRINT NON-DARE HIATUS INFO                   
*                                                                               
CMT70    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1C'        IS DARE AGY ORDER ELEM THERE?                
         BAS   RE,GETEL                                                         
         BNE   EXIT                NO, CONTINUE                                 
         USING RCONTKEL,R6                                                      
         GOTO1 VHEXOUT,DMCB,RCONTKCN,WORK,4                                     
         GOTO1 AADDDATA,DMCB,AFABLK,WKSDRVC#,WORK,0  VARIOUS CON #              
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
*********************************************************************           
* PRINT DARE AGENCY HIATUS DATES IF ANY                                         
*********************************************************************           
HIATUS   NTR1                                                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   HIATUSX                                                          
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETEL                                                         
         BNE   HIATUSX                                                          
         USING RCONHIEL,R6                                                      
                                                                                
         CLI   RCONHILN,2          SKIP IF NO DATES                             
         BNH   HIATUSX                                                          
                                                                                
         ZIC   R2,RCONHILN                                                      
         SH    R2,=H'2'            SUBTRACT OVERHEAD AND                        
         SRL   R2,1                DIVIDE BY 2 TO GET NUMBER OF ENTRIES         
                                                                                
         LA    R6,RCONHIDT                                                      
         DROP  R6                                                               
                                                                                
HIATUS50 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSHIADT,0(R6),0                            
         LA    R6,2(R6)                                                         
         BCT   R2,HIATUS50                                                      
                                                                                
HIATUSX  DS    0H                                                               
         B     EXIT                                                             
*********************************************************************           
* PRINT NON-DARE HIATUS DATES AND COMMENTS IF ANY                               
*********************************************************************           
NODAHIAT NTR1                                                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'25'        HIATUS DATES ELEMENT                         
         BAS   RE,GETEL                                                         
*                                                                               
NDHI050  DS    0H                                                               
         BNE   NDHI205                                                          
         LA    R4,2(R6)            FIRST DATE                                   
         ZIC   R2,1(R6)            LEN                                          
         AR    R2,R6               END OF ELEM                                  
*                                                                               
NDHI070  DS    0H                                                               
         CR    R4,R2                                                            
         BNL   NDHI100                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,WKSHIADT,0(R4),0                            
         SR    R3,R3                                                            
         ICM   R3,1,2(R4)            # OF DAYS                                  
         BZ    NDHI090                                                          
         GOTO1 VDATCON,DMCB,(2,0(R4)),(0,WORK)   MAKE YYMMDD                    
NDHI080  DS    0H                                                               
         GOTO1 VADDAY,DMCB,WORK,WORK,1          ADD +1 DAY                      
         GOTO1 VDATCON,DMCB,(0,WORK),(2,WORK+6) BACK TO COMPRESSED              
         GOTO1 AADDDATA,DMCB,AFABLK,WKSHIADT,WORK+6,0  WRITE DATE               
         BCT   R3,NDHI080                                                       
                                                                                
NDHI090  DS    0H                                                               
         LA    R4,3(R4)            NEXT MINI-ELEM                               
         B     NDHI070                                                          
*                                                                               
NDHI100  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         B     NDHI050                                                          
*                                                                               
* PRINT COMMENTS                                                                
NDHI205  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'26'                                                     
         BAS   RE,GETEL            ANY COMMENT ELEMENTS?                        
         BNE   NDHIX               NO, GO OUT                                   
*                                                                               
         USING RCONHCEL,R6                                                      
NDHI220  ZIC   R2,RCONHCLN         ELEMENT LENGTH                               
         LTR   R2,R2               ZERO LENGTH?                                 
         BZ    NDHI230             YES, DON'T PRINT ANYTHING                    
         GOTO1 AADDDATA,DMCB,AFABLK,WKSHIACM,2(R6),(R2)                         
         DROP  R6                                                               
*                                                                               
NDHI230  BAS   RE,NEXTEL           ANY MORE ELEMENTS?                           
         BE    NDHI220             YES                                          
*                                                                               
NDHIX    DS    0H                                                               
         B     EXIT                                                             
         LTORG                                                                  
*********************************************************************           
* ROUTINE TO PRINT AGENCY COMMENTS                                              
*********************************************************************           
AGYCOM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING RAGY2REC,R6                                                      
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY(6),RCONKAGY                                             
         MVC   RAGK2REP,REPALPHA                                                
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ACMX                                                             
         MVC   AIOREC,AIO3                                                      
         GOTO1 VGETREC,AIOREC                                                   
         L     R6,AIOREC                                                        
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL                                                         
ACM050   DS    0H                                                               
         BNE   ACMX                                                             
*                                                                               
         L     R3,AIO2                                                          
         GOTO1 =V(REGENSTC),DMCB,(2,(R6)),(R3),VDMGR,RCONREC,VGETTXT,  +        
               RR=Y                                                             
         BNZ   ACM080              COMMENT NOT FOUND                            
         CLI   0(R3),0             IF NULL, PRINT FREE FORM COMMENT             
         BE    ACM080                                                           
*                                                                               
ACM070   DS    0H                                                               
         CLI   0(R3),X'FF'         DONE                                         
         BE    ACM090                                                           
         ZIC   R2,0(R3)                                                         
         AHI   R2,-1                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,WKSAGCMT,1(R3),(R2)                         
         LA    R3,1(R2,R3)         NEXT LINE                                    
         C     R3,AIO3             BOUNDARY CHECK                               
         BH    ACM090              OUT OF BOUNDS                                
         B     ACM070              NEXT LINE                                    
*                                                                               
ACM080   DS    0H                                                               
         ZIC   R2,1(R6)                                                         
         AHI   R2,-2                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,WKSAGCMT,2(R6),(R2)                         
ACM090   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         B     ACM050                                                           
ACMX     DS    0H                                                               
         B     EXIT                                                             
         LTORG                                                                  
*********************************************************************           
* ROUTINE TO PRINT AGENCY COMMENTS                                              
*********************************************************************           
AGYRISK  NTR1  BASE=*,LABEL=*                                                   
         CLI   TWAARISK,0          PRINT IF CREDIT RISK OTHER THAN OK           
         BE    ARISKX              OR MISSING                                   
         CLI   TWAARISK,1                                                       
         BE    ARISKX                                                           
         CLI   TWAARISK,6          SHOULDN'T BE GREATER THAN 6,                 
         BH    ARISKX              IF IT IS, JUST EXIT                          
         LA    R2,RISKTAB          GET APPROPRIATE MESSAGE                      
         ZIC   RF,TWAARISK                                                      
         BCTR  RF,0                                                             
         MH    RF,=H'40'                                                        
         AR    R2,RF                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,WKSAGRSK,0(R2),0                            
*                                                                               
ARISKX   B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* PRINT AGENCY LIABILITY POSITION COMMENTS                                      
*********************************************************************           
AGYLIAB  NTR1  BASE=*,LABEL=*                                                   
         CLI   TWAALIAB,0          PRINT IF CREDIT RISK OTHER THAN OK           
         BE    ALBX                OR MISSING                                   
*                                                                               
         L     R3,AIO2                                                          
         GOTO1 =V(REGENSTC),DMCB,(4,TWAALIAB),(R3),VDMGR,RCONREC,      +        
               VGETTXT,RR=Y                                                     
         BNZ   ALBX                COMMENT NOT FOUND                            
         CLI   0(R3),0                                                          
         BE    ALBX                                                             
*                                                                               
ALB070   DS    0H                                                               
         CLI   0(R3),X'FF'         DONE                                         
         BE    ALBX                                                             
         ZIC   R2,0(R3)                                                         
         AHI   R2,-1                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,WKSAGLIA,1(R3),(R2)                         
         LA    R3,1(R2,R3)         NEXT LINE                                    
         C     R3,AIO3             BOUNDARY CHECK                               
         BH    ALBX                OUT OF BOUNDS                                
         B     ALB070              NEXT LINE                                    
*                                                                               
ALBX     B     EXIT                                                             
*********************************************************************           
* PRTCOV - SEND COVERSHEET LINES & CONTROLS                                     
*********************************************************************           
PRTCOV   NTR1  BASE=*,LABEL-*                                                   
         MVC   AIOREC,AIO3                                                      
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   COVER010                                                         
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BO    COVER020            YES - GEN COVERSHEET                         
         DROP  R6                                                               
*                                                                               
COVER010 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   COVER020                                                         
         USING RCONSEND,R6                                                      
         TM    RCONSENF,X'40'      LAST SENT BY STATION?                        
         BO    COVERX              YES - SKIP COVERSHEET                        
         DROP  R6                                                               
*                                                                               
COVER020 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A6'                                                     
         BAS   RE,GETEL                                                         
         BNE   COVERX                                                           
*                                                                               
         USING RCONCVEL,R6                                                      
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING RCOVREC,R2                                                       
         GOTOX (RFCONLOW,VREPFACS),DMCB,RCONREC  GET LOWEST K NUMBER            
         MVI   KEY,X'49'                                                        
         MVC   RCOVKREP,RCONKREP                                                
         MVC   RCOVKNAM,RCONCVNM                                                
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BNE   COVERX                                                           
         GOTO1 VGETREC,AIOREC                                                   
         L     R2,AIOREC                                                        
         DROP  R6                                                               
*                                                                               
         MVI   WPCOVFLG,0          INIT FLAGS                                   
         TM    RCOVFLAG,RCOVFLAF                                                
         BZ    COVER030                                                         
         OI    WPCOVFLG,X'80'      AUTO FORMAT ON                               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSCVCTL,=C'W',1                            
*                                                                               
COVER030 DS    0H                                                               
         LA    R6,RCOVEL1          POINT TO 1ST ELEM IN RCOVREC                 
         DROP  R2                                                               
*                                                                               
COVER050 DS    0H                                                               
         BAS   RE,GETCOV           GET NEXT COVER TEXT ELEM                     
         LTR   R6,R6               HAVE ANOTHER ELEMENT?                        
         BZ    COVERX              NO -WRAP IT UP                               
*                                                                               
         CLC   =C'SC=',2(R6)       SFM CMT?                                     
         BE    *+14                                                             
         CLC   =C'C=',2(R6)        STD CMT?                                     
         BNE   COVER054                                                         
         BAS   RE,STDCMT                                                        
         B     COVER050                                                         
*                                                                               
COVER054 DS    0H                                                               
         CLI   1(R6),2             PRINTING BLANK LINE?                         
         BH    COVER060            NO, NEXT CASE                                
         GOTO1 AADDDATA,DMCB,AFABLK,WKSCVCTL,=C'L',1                            
         B     COVER050            NEXT ELEMENT                                 
*                                                                               
COVER060 DS    0H                                                               
         ZIC   R3,1(R6)                                                         
         SH    R3,=H'2'            LENGTH FOR CURRENT DATA                      
         LA    R5,2(R6)            A(CURRENT DATA)                              
         MVC   FULL,0(R5)                                                       
         OC    FULL,SPACES         KEEP THE 1ST 4 CHAR IN CAPS                  
*                                                                               
         CH    R3,=H'2'            CHECK FOR '$P' PAGE BREAK                    
         BNE   COVER070                                                         
         CLC   =C'$P',FULL                                                      
         BNE   COVER070                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,WKSCVCTL,=C'P',1                            
         B     COVER050                                                         
*                                                                               
COVER070 DS    0H                  CHECK FOR '$ON' CONTROL                      
         CH    R3,=H'3'                                                         
         BNE   COVER080                                                         
         CLC   =C'$ON',FULL                                                     
         BNE   COVER080                                                         
         OI    WPCOVFLG,X'80'      AUTO FORMAT ON                               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSCVCTL,=C'W',1                            
         B     COVER050                                                         
*                                                                               
COVER080 DS    0H                  CHECK FOR '$OFF' CONTROL                     
         CH    R3,=H'4'                                                         
         BNE   COVER090                                                         
         CLC   =C'$OFF',FULL                                                    
         BNE   COVER090                                                         
         NI    WPCOVFLG,X'FF'-X'80' AUTO FORMAT OFF                             
         GOTO1 AADDDATA,DMCB,AFABLK,WKSCVCTL,=C'F',1                            
         B     COVER050                                                         
*                                                                               
COVER090 DS    0H                  HANDLE LINE TEXT                             
         GOTO1 AADDDATA,DMCB,AFABLK,WKSCVTXT,0(R5),(R3)                         
         B     COVER050            NEXT ELEMENT                                 
COVERX   XIT1                                                                   
*                                                                               
***********************************************************************         
* GETCOV - POINTS R6 TO NEXT ELEMENT IN RECORD SET R6=0 WHEN END OF REC         
***********************************************************************         
GETCOV   DS    0H                                                               
         ST    RE,FULL             SAVE RE FOR RETURN                           
*                                                                               
         MVI   ELCODE,3                                                         
         BAS   RE,NEXTEL           NEXT TEXT ELEM                               
         BE    GETCOVX             GOT IT, ALL DONE                             
*                                                                               
         L     R1,AIOREC                                                        
         USING RCOVREC,R1                                                       
         MVC   KEY(27),RCOVKEY     GET NEXT COVER RECORD IN SET                 
         ZIC   RE,RCOVKSEQ                                                      
         LA    RE,1(RE)                                                         
         STC   RE,KEY+26                                                        
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     ANOTHER RECORD IN SET?                       
         BE    GETCOV30            YES - PROCESS IT                             
         SR    R6,R6               NO - RETURN R6=0                             
         B     GETCOVX             RETURN                                       
*                                                                               
GETCOV30 DS    0H                                                               
         GOTO1 VGETREC,AIOREC                                                   
         L     R1,AIOREC                                                        
         LA    R6,RCOVEL1          1ST ELEM IN REC                              
         CLI   0(R6),3                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
GETCOVX  DS    0H                                                               
         L     RE,FULL                                                          
         BR    RE                                                               
         DROP  R1                                                               
         B     COVERX                                                           
*                                                                               
* EXPLODE STANDARD COMMENTS                                                     
*                                                                               
STDCMT   NTR1                                                                   
         LA    R3,CMTIO                                                         
         GOTO1 =V(REGENSTC),DMCB,(2,(R6)),(R3),VDMGR,RCONREC,VGETTXT,  +        
               RR=Y                                                             
         BZ    STD070              COMMENT FOUND                                
         CLI   0(R3),0             IF NULL, PRINT FREE FORM COMMENT             
         BNE   STDX                                                             
         ZIC   R2,1(R6)                                                         
         AHI   R2,-2                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,WKSCVTXT,2(R6),(R2)                         
         B     STDX                                                             
*                                                                               
STD070   DS    0H                                                               
         CLI   0(R3),X'FF'         DONE                                         
         BNE   STD075                                                           
         ZIC   R1,1(R6)                                                         
         AR    R6,R1               NEXT ELEM                                    
         B     STDX                                                             
STD075   DS    0H                                                               
         ZIC   R2,0(R3)                                                         
         AHI   R2,-1                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,WKSCVTXT,1(R3),(R2)                         
         LA    R3,1(R2,R3)         NEXT LINE                                    
         LA    RF,CMTIO                                                         
         AHI   RF,L'CMTIO                                                       
         CR    R3,RF               BOUNDARY CHECK                               
         BH    STDX                OUT OF BOUNDS - GET OUT                      
         B     STD070              NEXT LINE                                    
*                                                                               
STDX     XIT1                                                                   
         LTORG                                                                  
*          DATA SET RECNT63    AT LEVEL 059 AS OF 03/08/02                      
*********************************************************************           
*  ONLY PRINT SPL DATA ON VER 1, OR IF IT'S UPDATED SINCE LAST SEND             
*********************************************************************           
DOSPL    NTR1  BASE=*,LABEL=*                                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   DSPL05                                                           
         LR    R2,R6                                                            
*                                  SPL ELEMENT X'06'                            
         OC    5(3,R2),5(R2)       NO SPL ENTRY DATE INDICATES                  
         BZ    DSPL05              NO SPL DOLLARS                               
         TM    RCONSPES-RCONSPEL(R2),X'04'                                      
*                                  DOLLARS OR PERCENTS?                         
         BNO   PCON152             NOT ON = DOLLARS                             
*                                     TREAT AS ORIGINALLY                       
         GOTO1 =A(NEWSPL),DMCB,(RC),RR=Y                                        
*                                  ON  =  PERCENTS                              
*                                        ISOLATE DOLLARS                        
PCON152  EQU   *                                                                
         SR    R6,R6                                                            
         LA    R5,STALST                                                        
         MVI   BYTE2,0                                                          
         TM    4(R2),X'80'         STA WON 100% OF MKT?                         
         BZ    *+8                                                              
         MVI   BYTE2,1             SAVE FOR DOSPL (RADIO PART)                  
         SR    RE,RE                                                            
         CLI   8(R2),6             MORE THAN 6 STAS?                            
         BNH   *+12                NO, USE ACTUAL                               
         LA    RE,6                ELSE LIMIT TO SIX                            
         B     *+8                                                              
         IC    RE,8(R2)            NUMBER OF MINI ELEMENTS                      
         LR    R4,R2                                                            
         LA    R4,9(R2)            POINT TO MINI ELEMENT                        
PCON160  MVC   0(5,R5),0(R4)       MOVE IN STATION                              
         OC    0(5,R5),SPACES                                                   
         MVC   5(4,R5),5(R4)       MOVE IN AMOUNT                               
         L     R0,5(R4)                                                         
         AR    R6,R0               ADD STATION AMT TO MKT TOTAL                 
         LA    R4,9(R4)                                                         
         LA    R5,9(R5)                                                         
         BCT   RE,PCON160                                                       
*                                                                               
         ST    R6,MKTOT            TOTAL DOLLARS IN MARKET                      
         TM    RCONSPES-RCONSPEL(R2),X'04'                                      
*                                  DOLLARS OR PERCENTS?                         
         BNO   DSPL05              NOT ON = DOLLARS                             
*                                     TREAT AS ORIGINALLY                       
         MVC   MKTOT,EST$$$        ON  = PERCENTS:                              
*                                     USE ESTIMATE DOLLARS                      
DSPL05   DS    0H                                                               
         GOTO1 =A(DOXSPL),RR=Y                                                  
*                                                                               
* IS THERE SPL DATA TO PRINT?                                                   
         OC    STALST,STALST                                                    
         BNZ   DSPL10                                                           
         TM    TWAPRFK,X'20'  SKIP LINE WHEN CONTYPE K OPTION#3 ON              
         BO    DSPL80                                                           
         GOTO1 AADDDATA,DMCB,AFABLK,WKSSPLNA,0,0                                
         B     DSPL80                                                           
* PRINT SPL DATA                                                                
DSPL10   DS    0H                                                               
         L     R2,MKTOT                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,WKSSPLTT,MKTOT,0                            
*                                                                               
*MNS                                                                            
         GOTO1 =A(COMPVAL),RR=Y                                                 
*MNE                                                                            
*                                                                               
DSPL80   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'07'        SPL/EPL COMMENT ELEMENT                      
         BAS   RE,GETEL                                                         
DSPL90   BNE   DSPL100             ELEMENT FOUND?                               
         ZIC   R5,1(R6)                                                         
         AHI   R5,-2                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,WKSSPLCM,2(R6),(R5)                         
         BAS   RE,NEXTEL                                                        
         B     DSPL90                                                           
DSPL100  EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
         LTORG                                                                  
*          DATA SET RECNT63    AT LEVEL 059 AS OF 03/08/02                      
**********************************************************************          
* PROFILE TO PRINT FORECAST/BUDGET$$                                            
**********************************************************************          
DOXSPL   NTR1  BASE=*,LABEL=*                                                   
         TM    TWATIME,X'20'                                                    
         BZ    DOXSPLX                                                          
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,9            GET EXTRA SPL ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   DOXSPLX                                                          
                                                                                
         USING RCONSXEL,R6                                                      
         OC    RCONSXFD,RCONSXFD                                                
         BNZ   DOXS10                                                           
         TM    RCONSXFG,X'40'                                                   
         BZ    DOXS20                                                           
                                                                                
DOXS10   DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSSPLFC,RCONSXFD,0                         
                                                                                
DOXS20   DS    0H                                                               
         OC    RCONSXBD,RCONSXBD                                                
         BNZ   DOXS30                                                           
         TM    RCONSXFG,X'20'                                                   
         BZ    DOXSPLX                                                          
                                                                                
DOXS30   DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSSPLBD,RCONSXBD,0                         
         DROP  R6                                                               
                                                                                
DOXSPLX  DS    0H                                                               
         B     EXIT                                                             
         LTORG                                                                  
*********************************************************************           
*        NEWSPL --- PROCESS SPL INFORMATION WITH NEW FORMAT                     
*             R2  =  A(SPL ELEMENT IN RCONREC)                                  
*********************************************************************           
NEWSPL   NMOD1 0,*NEWSPL*                                                       
         L     RC,0(R1)            RELOAD A(WORKSPACE)                          
         LA    R1,RCONELEM         ACCUM $$ FROM ESTIMATES                      
         SR    R7,R7                                                            
NEWS0010 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    NEWS0040            YES                                          
         CLI   0(R1),3             ESTIMATE?                                    
         BE    NEWS0030            YES                                          
NEWS0020 EQU   *                                                                
         ZIC   R3,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R3                                                            
         B     NEWS0010            GO BACK FOR NEXT                             
NEWS0030 EQU   *                                                                
         A     R7,6(R1)            ACCUMULATE ESTIMATE $$                       
         B     NEWS0020                                                         
NEWS0040 EQU   *                                                                
         SR    R6,R6                                                            
         D     R6,=F'100'          GET RID OF PENNIES                           
         ST    R7,EST$$$           SAVE RESULT                                  
*                                                                               
         TM    RCONSPES-RCONSPEL(R2),X'40'                                      
*                                  NO REP'D STA:  VALUE = TOTAL?                
         BNO   NEWS0050            NO                                           
         BAS   RE,PERCDOLS         YES - RETRIEVE $$ FROM X'08' ELT             
         B     NEWS0070                                                         
NEWS0050 EQU   *                                                                
         TM    RCONSPES-RCONSPEL(R2),X'20'                                      
*                                  REP STATION OVERRIDE $$?                     
         BNO   NEWS0060            NO                                           
         BAS   RE,PERCDOLS         YES - RETRIEVE $$ FROM X'08' ELT             
NEWS0060 EQU   *                                                                
         GOTO1 CALCTOT$,DMCB,(R2)                                               
*                                  CALC TOTAL $$ FROM COMPONENTS                
NEWS0070 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   AT THIS POINT, EST$$$ CONTAINS TOTAL MARKET BUDGET, EITHER                  
*      CALCULATED FROM REP'D STATION PERCENT AND $$, OR AS A FIXED              
*      FIGURE BECAUSE REP'D STATION HAS NO PERCENT VALUE                        
*                                                                               
*   PERCDOLS:  RETRIEVE X'08' ELEMENT, GET DOLLARS FROM IT                      
*                                                                               
PERCDOLS NTR1                                                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'08'        SET FOR X'08' ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    PDOL0010            FOUND                                        
         DC    H'0'                MUST BE FOUND!!                              
PDOL0010 EQU   *                                                                
         MVC   EST$$$,RCONAC$$-RCONACEL(R6)                                     
*                                  UNLOAD DOLLARS                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CALCTOT$:  CALCULATE THE TOTAL MARKET DOLLARS FROM STATION                  
*      DOLLARS AND PERCENT                                                      
*                                                                               
CALCTOT$ NTR1                                                                   
         L     R2,0(R1)            RELOAD A(X'06' ELEMENT)                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
* RETURN 0 IF DIVISION BY ZERO                                                  
         OC    RCONSPAM-RCONSPEL(4,R2),RCONSPAM-RCONSPEL(R2)                    
         BZ    CALCTOTX                                                         
*                                                                               
         L     RF,EST$$$           LOAD ESTIMATE DOLLARS                        
         M     RE,=F'10000'                                                     
*                                                                               
*   10,000 PROPERLY DECIMAL-ALIGNS A PERCENT VALUE OF FORMAT X.XX%              
*                                                                               
         SLDA  RE,1                DOUBLE FOR ROUNDING                          
         A     RF,RCONSPAM-RCONSPEL(R2)                                         
*                                  PERCENT FROM 1ST MINI-ELEMENT IN             
*                                     X'06' ELT FOR ROUNDING                    
         D     RE,RCONSPAM-RCONSPEL(R2) DIVIDE BY PERCENT                       
         SRA   RF,1                DIVIDE BY 2                                  
*                                                                               
CALCTOTX DS    0H                                                               
         ST    RF,EST$$$           STORE IT BACK                                
         XIT1                                                                   
         LTORG                                                                  
*********************************************************************           
*        CALCULATE MARKET TOTALS                                                
*********************************************************************           
*MNS                                                                            
MKTL     NTR1  BASE=*,LABEL=*                                                   
         ZAP   DIVIDEND,=P'0'                                                   
         ZAP   DUB,=P'0'                                                        
                                                                                
         XC    FULL2,FULL2                                                      
         SR    R5,R5                                                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   MKTLEX                                                           
*MN      BE    *+6                                                              
*MN      DC    H'0'                                                             
MKTL020  L     R2,6(R6)                                                         
         AR    R5,R2                                                            
         BAS   RE,NEXTEL                                                        
         BE    MKTL020                                                          
         CVD   R5,DUB                                                           
         ZAP   DIVIDEND(16),DUB(8)                                              
         MP    DIVIDEND,=P'10000'                                               
                                                                                
MKTL100  EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   MKTLEX                                                           
         USING RCONSPEL,R6                                                      
         MVC   FULL2,RCONSPAM                                                   
         L     R5,FULL2                                                         
         CVD   R5,DUB                                                           
         ZAP   DIVIDER(4),DUB+4(4)                                              
         CP    DIVIDER,=P'0'                                                    
         BNE   MKTL150                                                          
         ZAP   MRKTOTL(10),=P'10000'                                            
         B     MKTL200                                                          
         DROP  R6                                                               
                                                                                
MKTL150  EQU   *                                                                
         DP    DIVIDEND,DIVIDER                                                 
         SRP   DIVIDEND(12),62,5                                                
         ZAP   MRKTOTL(10),DIVIDEND(12)   TOTAL DOLLARS IN MARKET               
                                                                                
MKTL200  EQU   *                                                                
         GOTO1 AADDDATA,DMCB,AFABLK,WKMKTTOT,MRKTOTL,0                          
                                                                                
MKTLEX   DS    0H                                                               
                                                                                
         XIT1                                                                   
         LTORG                                                                  
                                                                                
DIVIDER  DS    F                                                                
DIVIDEND DS    PL16                                                             
*MNE                                                                            
*MNS                                                                            
*********************************************************************           
*        CALCULATE INVIDUAL COMPETITIVE MARKET                                  
*********************************************************************           
COMPVAL  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R6,RCONREC                                                       
         USING RCONSPEL,R6                                                      
         MVI   ELCODE,6            GET EPL STATIONS                             
         BAS   RE,GETEL                                                         
         BNE   COMPXIT                                                          
         CLI   RCONSPNU,0          ANY STATIONS??                               
         BE    COMPXIT                                                          
*                                                                               
         CLI   RCONSPNU,1          THIS MEANS IT DOESN'T HAVE ANY               
         BE    COMPXIT             STATIONS                                     
         ZIC   R5,RCONSPNU         NUMBER OF STATIONS                           
         LA    R4,RCONSPST         POINT TO MINI ELEMENT                        
*                                                                               
COMP050  DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSSPLST,0(R4),0                            
                                                                                
         TM    RCONSPES,X'04'                                                   
         BO    COMP070                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,WKSSPL$$,5(R4),0                            
                                                                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         SR    R2,R2                                                            
         ICM   R1,15,5(R4)          $$                                          
         ICM   R2,15,MKTOT          MARKET TOTAL                                
         DR    R0,R2                $$/MARKET TOTAL                             
         MH    R1,=H'10'                                                        
         A     R1,=F'500'                                                       
         MH    R1,=H'1000'                                                      
         ST    R1,CMPFULL           = %%                                        
                                                                                
         GOTO1 AADDDATA,DMCB,AFABLK,WKSSPLPC,CMPFULL,0                          
         B     COMP150                                                          
                                                                                
COMP070  DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSSPLPC,5(R4),0                            
                                                                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         SR    R2,R2                                                            
         ICM   R1,15,5(R4)          %%                                          
         ICM   R2,15,MKTOT          MARKET TOTAL                                
         MR    R0,R2                %%(MARKET TOTAL)                            
         A     R1,=F'5000'                                                      
         D     R0,=F'10000'                                                     
         ST    R1,CMPFULL           = MARKET SHARE                              
                                                                                
         GOTO1 AADDDATA,DMCB,AFABLK,WKSSPL$$,CMPFULL,0                          
*                                                                               
COMP150  DS    0H                                                               
         LA    R4,9(R4)            BUMP TO NEXT STATION                         
         BCT   R5,COMP050                                                       
         DROP  R6                                                               
                                                                                
COMPXIT  EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
                                                                                
CMPFULL  DS    F                                                                
*MNE                                                                            
*          DATA SET RECNT63    AT LEVEL 059 AS OF 03/08/02                      
*********************************************************************           
*  SEND BOP DATA                                                                
*********************************************************************           
DOBOP    NTR1  BASE=*,LABEL=*                                                   
*                                    PRINT A BLANK LINE                         
         LA    R3,RTGS                                                          
DBOP5    CLC   RCONRTGS,0(R3)                                                   
         BE    DBOP10                                                           
         LA    R3,L'RTGS(R3)                                                    
         CLI   0(R3),0                                                          
         BNE   DBOP5                                                            
         B     DBOP11                                                           
DBOP10   DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSBRTGS,0(R3),0                            
DBOP11   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   BBOPX                                                            
         USING RCONBPEL,R6                                                      
         MVC   WORK(6),RCONBBKS                                                 
         CLC   WORK(6),SPACES   NOW SUFFIX BOOK W/BOOK TYPE                     
         LA    R3,WORK+6                                                        
         BCTR  R3,0                                                             
         CLI   0(R3),C' '                                                       
         BE    *-6                                                              
         OI    RCONBBKT,C' '                                                    
         CLI   RCONBBKT,C' '                                                    
         BE    DBOP12                                                           
         MVC   1(3,R3),=C'( )'                                                  
         MVC   2(1,R3),RCONBBKT                                                 
         LA    R3,3(R3)                                                         
DBOP12   DS    0H                                                               
         LA    R3,1(R3)                                                         
         LA    RF,WORK                                                          
         SR    R3,RF                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,WKSBBOOK,WORK,(R3)                          
DBOP15   DS    0H                                                               
         CLI   RCONBPDM,X'FF'      VALIDATED BY DEMOVAL?                        
         BNE   DBOP45                                                           
         LA    R3,RCONBPDM+1       DEMOS                                        
         LA    R5,2                MAXIMUM 2 DEMOS                              
         LA    R4,WORK2            AREA FOR DBLOCK                              
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'R'                                                    
         DROP  R4                                                               
*                                                                               
DBOP20   OC    0(3,R3),0(R3)                                                    
         BZ    DBOP45                                                           
         CLI   1(R3),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R3),C'I'                                                       
         MVC   WORK(20),SPACES                                                  
         GOTO1 VDEMOCON,DMCB,(0,(R3)),(6,WORK),(0,WORK2)                        
         CLI   1(R3),C'I'                                                       
         BNE   *+8                                                              
         MVI   1(R3),C'T'                                                       
         LA    R4,WORK+8                                                        
*        - IF PRIMARY DEMO, PUT P AT END OF DEMO EXPRESSION                     
         TM    0(R3),X'40'         PRIMARY DEMO                                 
         BNO   DBOP40                                                           
         LR    R1,R4                                                            
DBOP30   BCTR  R1,0                                                             
         CLI   0(R1),C' '                                                       
         BE    DBOP30                                                           
         MVI   1(R1),C'P'                                                       
DBOP40   LA    R3,3(R3)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,WKSBDEMO,WORK,0                             
         BCT   R5,DBOP20                                                        
DBOP45   DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,WKSBMKT,RCONBPMK,0                          
*                                                                               
BBOPX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
* RATING SERVICE TABLE                                                          
RTGS     DS    0CL3                                                             
         DC    C'ARBNSISRCBIRTRCMTDRAM'                                         
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012REWCF11   05/04/06'                                      
         END                                                                    
