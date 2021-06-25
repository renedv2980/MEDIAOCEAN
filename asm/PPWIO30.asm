*          DATA SET PPWIO30    AT LEVEL 018 AS OF 02/20/15                      
*PHASE T41E30A                                                                  
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST'                                     
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               CHANGE LOG                                            *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 2                                                                
*                                                                               
*FEB/15        KWAN                FIX BAD BRANCH (LNKCURSQ)                    
*JUL/04        BOBY                BIG BANG                                     
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST'                                     
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               T41E30 - DETAIL MAINT/LIST                            *         
*                                                                     *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41E00 (WIO CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, CHG, DISP, SEL, LIST, DEL               *         
*                                                                     *         
*  INPUTS       SCREEN T41EFC (MAINTENANCE)                           *         
*               SCREEN T41EFA (LIST)                                  *         
*                                                                     *         
*  OUTPUTS                                                            *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- FIELD ON SCREEN                                 *         
*               R3 -- WORK                                            *         
*               R4 -- VARIOUS RECORDS                                 *         
*               R5 -- WORK                                            *         
*               R6 -- ELEMENTS IN RECORDS                             *         
*               R7 -- MINIO SET                                       *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- BASE REGISTER                                   *         
*               RC -- GEND                                            *         
*               RD -- REGISTER CHAIN                                  *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS                                                          *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST - INIT'                              
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T41E30   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41E30,RR=RE                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         L     R9,ASYSD                                                         
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         ST    RE,RELO20           SAVE RELOCATION FACTOR                       
*                                                                               
         GOTOR MININIT             INIT MINIO BLOCK                             
*                                                                               
         MVI   ERROR,0             CLEAR OLD STYLE ERROR CODE                   
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST - CKMODE'                            
***********************************************************************         
*                                                                     *         
*        DETERMINE CALLING MODE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CKMODE   DS    0H                                                               
*                                                                               
         CLI   MODE,VALKEY         VALKEY?                                      
         BNE   CKMODVKX                                                         
*                                                                               
         CLI   ACTNUM,ACTLIST         LIST SCREEN                               
         BNE   *+12                                                             
         BRAS  RE,VKL                                                           
         B     CKMODEX                                                          
*                                                                               
         BRAS  RE,VK                                                            
*                                                                               
         B     CKMODEX                                                          
*                                                                               
CKMODVKX DS    0H                                                               
*                                                                               
         CLI   MODE,PROCPFK        IF PFKEY HIT                                 
         BNE   CKMDPFKX                                                         
*                                                                               
         CLI   PFAID,12            IF PF KEY 12                                 
         BE    *+8                                                              
         CLI   PFAID,24            OR 24                                        
         BNE   CKMDPFKX                                                         
*                                                                               
         OI    GENSTAT2,NEXTSEL         GO TO NEXT SELECT                       
         NI    GENSTAT2,X'FF'-RETEQSEL  NOT SAME SCEEEN                         
         OI    GENSTAT2,DISTHSPG        DON'T SKIP TO NEXT LIST PAGE            
*                                                                               
         B     CKMODEX                                                          
*                                                                               
CKMDPFKX DS    0H                                                               
*                                                                               
         CLI   MODE,VALREC         VALREC?                                      
         BNE   CKMDVRN                                                          
*                                                                               
         CLI   ACTNUM,ACTABADD     IF ADDING STATUSES                           
         BNE   CKMDVR30                                                         
*                                                                               
         CLI   DDLNKSW,C'Y'        AND A LINK CALL                              
         BNE   CKMDVR30                                                         
*                                                                               
         BRAS  RE,ABADD               ADD SEVERAL STATUSES                      
                                                                                
         B     CKMODEX                                                          
*                                                                               
CKMDVR30 DS    0H                                                               
*                                                                               
         BRAS  RE,VR                                                            
*                                                                               
         B     CKMODE10                                                         
*                                                                               
CKMDVRN  DS    0H                                                               
*                                                                               
         CLI   MODE,LISTRECS       LIST SCREEN                                  
         BNE   *+12                                                             
         BRAS  RE,LR                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,DISPREC        DISREC?                                      
         BNE   *+12                                                             
         BRAS  RE,DR                                                            
         B     CKMODE10                                                         
*                                                                               
         CLI   MODE,DISPKEY        DISKEY?                                      
         BNE   *+12                                                             
         BRAS  RE,DK                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,RECDEL         RECDEL?                                      
         BNE   *+12                                                             
         BRAS  RE,DL                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   *+12                                                             
         BRAS  RE,LR                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   *+12                                                             
         BRAS  RE,PR                                                            
         B     CKMODEX                                                          
*                                                                               
CKMODE10 DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTSEL       IF SELECTING                                 
         BNE   *+8                                                              
         OI    GENSTAT2,RETEQSEL      RETURN TO THIS SCREEN                     
*                                                                               
         B     CKMODEX                                                          
*                                                                               
CKMODEX DS     0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST - VK'                                
***********************************************************************         
*                                                                     *         
*        VALIDATE KEY FIELDS                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VK       NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
*        HISTORY DOWNLOAD GIVES FAKE KEY FOR GENCON                             
*              PROCESSING IS DONE IN VR                                         
*                                                                               
         CLI   ACTNUM,ACTHIST      IF  IO DOWNLOAD                              
         BNE   VKHISTN                                                          
*                                                                               
         BRAS  RE,TRDATTP             TRANSLATE DATA TYPES                      
*                                                                               
*        FIND ANY KEY ON FILE TO SATISFY GENCON                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH WEB IO KEY                         
         USING WIOKEY,R4                                                        
*                                                                               
         MVC   WIOKAGY,QAGY        SET AGENCY                                   
         MVC   WIOKMED,QFMED       SET MEDIA                                    
*                                                                               
         GOTOR HIGH                READ DIRECTORY                               
*                                                                               
         MVC   QIOKEY,KEY          SAVE KEY INTO QIOKEY                         
*                                                                               
         B     VKX                                                              
*                                                                               
VKHISTN  DS    0H                                                               
*                                                                               
*        HANDLE REQUESTS FOR STATUS DOWNLOAD AND ADD                            
*                                                                               
VKST     DS    0H                                                               
*                                                                               
         CLI   DDLNKSW,C'Y'        IF LINK CALL                                 
         BNE   VKSTN                                                            
*                                                                               
         CLI   ACTNUM,ACTSTAT      IF  STATUS INQUIRY                           
         BNE   *+8                                                              
         BRAS  RE,TRDATTP             TRANSLATE DATA TYPES                      
*                                                                               
         CLI   ACTNUM,ACTSTAT      IF  STATUS INQUIRY                           
         BE    *+8                                                              
         CLI   ACTNUM,ACTABADD     OR  STATUS ADD                               
         BNE   VKSTN                                                            
*                                                                               
         BRAS  RE,FILLKEY             FILL IN KEY WITH NEXT IO KEY              
*                                                                               
         CLI   LNKSTSW,C'L'        DONE IF END OF LIST OF KEYS                  
         BE    VKX                                                              
*                                                                               
         CLI   ACTNUM,ACTSTAT      IF  STATUS INQUIRY                           
         BNE   *+8                                                              
         LHI   RF,E#IOIQRP            SET APPROPRIATE RETURN CODE               
*                                                                               
         CLI   ACTNUM,ACTABADD     OR  STATUS ADD                               
         BNE   *+8                                                              
         LHI   RF,E#IOSTRP            SET APPROPRIATE RETURN CODE               
*                                                                               
         STCM  RF,3,LNKREPCD       SET LINK REPLY CODE                          
*                                                                               
VKSTN    DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTABDEL     IF ACTION ABDELETE                           
         BNE   *+8                                                              
         BRAS  RE,DK                  DISPLAY THE KEY FIRST                     
*                                                                               
         MVI   CHGSWTCH,0          INITIALIZE CHANGE SWITCH                     
         MVI   DATAFLDS,0          INITIALIZE FIELDS WITH DATA                  
*                                                                               
*        VALIDATE MEDIA                                                         
*                                                                               
         LA    R2,SSTMEDH          POINT TO MEDIA FIELD                         
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         GOTOR VALMED              VALIDATE MEDIA                               
*                                                                               
*        VALIDATE CLIENT                                                        
*                                                                               
         LA    R2,SSTCLTH          POINT TO CLIENT FIELD                        
*                                                                               
         GOTOR VALCLT              VALIDATE CLIENT                              
*                                                                               
*        VALIDATE PUB                                                           
*                                                                               
         LA    R2,SSTPUBH          POINT TO PUB FIELD                           
*                                                                               
         GOTOR VALPUB              VALIDATE PUB                                 
*                                                                               
*        VALIDATE INSORD NUMBER                                                 
*                                                                               
         LA    R2,SSTIO#H          POINT TO INSORD FIELD                        
*                                  REQUIRED                                     
         GOTOR VALIO#              VALIDATE WEB IO #                            
*                                                                               
         OI    DATAFLDS,DFLIO#Q    IO#    ENTERED                               
*                                                                               
VKIO#X   DS    0H                                                               
*                                                                               
*        VALIDATE REVISION #                                                    
*                                                                               
VKRV#    DS    0H                                                               
*                                                                               
         LA    R2,SSTREV#H         POINT TO REVISION FIELD                      
*                                                                               
         XC    QREV#,QREV#                                                      
*                                                                               
         CLI   FLDILEN,0           OKAY IF REVISON # MISSING                    
         BE    VKRV#X                                                           
*                                                                               
         GOTOR VALRV#              VALIDATE REVISION #                          
*                                                                               
         OI    DATAFLDS,DFLRV#Q    RV#    ENTERED                               
*                                                                               
VKRV#X   DS    0H                                                               
*                                                                               
*        VALIDATE PERIOD                                                        
*                                                                               
VKPER    DS    0H                                                               
*                                                                               
         LA    R2,SSTPERH            PERIOD                                     
*                                                                               
         CLI   FLDILEN,0             PERIOD IS NOT REQUIRED                     
         BE    VKPERX                                                           
*                                                                               
         OC    SSTPER,SPACES         MAKE UPPERCASE                             
*                                                                               
         GOTOR VALPER                VALIDATE PERIOD                            
*                                                                               
         OI    DATAFLDS,DFLPERQ    PERIOD ENTERED                               
*                                                                               
         LA    R3,WORK               PERVAL OUTPUT AREA                         
         USING PERVALD,R3                                                       
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OLD DATA                               
*                                                                               
         MVC   FLDDATA(L'PVALCPER),PVALCPER                                     
*                                                                               
         DROP  R3                                                               
*                                                                               
VKPERX   DS    0H                                                               
*                                                                               
*        ALL KEYFIELDS VALIDATED                                                
*                                                                               
*        ANALYZE WHICH FIELDS ENTERED                                           
*                                                                               
         TM    DATAFLDS,DFLIO#Q+DFLRV#Q IF IO# AND REVISION GIVEN               
         B     VKKEY                       GO BUILD KEY                         
*                                                                               
*        SKIPPED FOR THE MOMENT                                                 
*                                                                               
         TM    DATAFLDS,DFLRV#Q    IF RV# WITHOUT IO#                           
         BO    VKRV#ER                ERROR                                     
*                                                                               
         TM    DATAFLDS,DFLSSQNQ   SEQ NUMBER MUST BE MISSING                   
         BO    VKSSQNER                                                         
*                                                                               
         TM    DATAFLDS,DFLIO#Q    IF NO IO#                                    
         BO    *+12                                                             
         TM    DATAFLDS,DFLPERQ       MUST HAVE PERIOD                          
         BNO   VKPERER                                                          
*                                                                               
         GOTOR FNDIO#,DMCB,QSTART  FIND IO# AND MOST RECENT RV#                 
         BZ    *+6                                                              
         DC    H'0'                NOT FOUND                                    
*                                                                               
         LA    R2,SSTIO#H          POINT TO IO# FIELD                           
*                                                                               
         GOTOR DISIO#              DISPLAY IO#                                  
*                                                                               
         LA    R2,SSTREV#H         POINT TO REVISION FIELD                      
*                                                                               
         GOTOR DISRV#              DISPLAY RV#                                  
*                                                                               
*        BUILD KEY FOR GENCON                                                   
*                                                                               
VKKEY    DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH WEB IO KEY                         
         USING WIOKEY,R4                                                        
*                                                                               
         MVC   WIOKAGY,QAGY        SET AGENCY                                   
         MVC   WIOKMED,QMED        SET MEDIA                                    
         MVI   WIOKRCD,WIOKRCDQ    SET RECORD CODE                              
         MVC   WIOKCLT,QCLT        SET CLIENT                                   
         MVC   WIOKPUB,QPUB        SET PUB CODE                                 
*                                                                               
         MVC   WIOKIOYR,QIO#IOYR   SET PERIOD YEAR                              
         MVC   WIOKIOSQ,QIO#IOSQ   SET SEQUENCE NUMBER                          
         MVC   WIOKRV#,QREV#       SET REVISION NUMBER                          
         MVC   WIOKELMK,=8X'FF'    ONLY INTERESTED IN MASTER KEYS               
*                                                                               
         MVC   QIOKEY,KEY          SAVE KEY INTO QIOKEY                         
*                                                                               
*        OPEN MINIO SET                                                         
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH INSORD MASTER KEY                  
         USING WIOKEY,R4                                                        
*                                                                               
         MVC   WIOKEY,KEY          SET KEY                                      
         MVI   MINDELSW,C'Y'       READ DELETED MINIO SETS                      
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,MINESNF      ERROR IF RECORD SET NOT FOUND                
         BNE   VKKEY05                                                          
*                                                                               
         CLI   ACTNUM,ACTSTAT      ERROR IF NOT STATUS INQUIRY                  
         BNE   VKIO#ER                                                          
*                                                                               
         MVI   WIOKZONE,0          CLEAR ZONE                                   
         MVI   WIOKEDN,0           AND EDITION                                  
*                                  CHECK FOR BASE PUB IEIO                      
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,MINESNF      ERROR IF RECORD SET NOT FOUND                
         BE    VKIO#2ER                                                         
*                                                                               
         MVC   QIOKEY,WIOKEY       SAVE KEY INTO QIOKEY                         
*                                                                               
VKKEY05  DS    0H                                                               
*                                                                               
         CLI   MINERR,0            NO OTHER ERRORS TOLERATED                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        FIND HEADER ELEMENT AND DISPLAY PERIOD                                 
*                                                                               
         TM    MINSTAT,MINDELQ     SKIP IF MINIO SET DELETED                    
         BO    VKX                                                              
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR ELEMENT WORKAREA                       
*                                                                               
         LA    R6,ELEMENT          BUILD HEADER ELEMENT KEY                     
         USING WIOHKEY,R6                                                       
*                                                                               
         MVI   WIOHKCDE,WIOHKIDQ   SET HEADER ELM CODE                          
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT READ FOR ELEMENT                             
         BNE   VKIO#2ER            MUST BE DELETED                              
         BE    *+6                 MUST FIND ELEMENT                            
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
         MVC   SVHDRELM,WIOHDRD    SAVE HEADER ELEMENT                          
*                                                                               
         LA    R2,SSTPERH          POINT TO PERIOD FIELD                        
         MVC   QPER,WIOHSTRT       SAVE PERIOD                                  
*                                                                               
         GOTOR DISPER,DMCB,QPER    DISPLAY PERIOD                               
*                                                                               
*        VALIDATE STATUS SEQ NUMBER                                             
*                                                                               
VKSSQN   DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTSTAT      DONE IF ACTION STATUS                        
         BE    VKX                                                              
*                                                                               
         MVI   QSSQN,0             INIT SEQ NUMBER                              
*                                                                               
         LA    R2,SSTSQNH          POINT TO SEQ NUMBER FIELD                    
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        GET INPUT LENGTH                             
         BZ    VKSSQNX             DONE IF NO INPUT                             
*                                                                               
         TM    FLDIIND,FINPNUM     INPUT MUST BE NUMERIC                        
         BNO   VKSSQNER                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLDDATA(0)      PACK SEQ NUMBER                              
*                                                                               
         CVB   RF,DUB              CVB                                          
*                                                                               
         CHI   RF,256              MUST BE LT 256                               
         BNL   VKSSQNER                                                         
*                                                                               
         STC   RF,QSSQN            SAVE SEQ NUMBER                              
*                                                                               
         OI    DATAFLDS,DFLSSQNQ   SEQ NUMBER ENTERED                           
*                                                                               
VKSSQNX  DS    0H                                                               
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          BUILD ELEMENT KEY                            
         USING WIOSKEY,R6                                                       
*                                                                               
         MVI   WIOSKCDE,WIOSKIDQ   SET STATUS ELM CODE                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,QSSQN           GET SEQ NUMBER                              
         BNZ   VKSSQN10               NUMBER SPECIFIED                          
*                                    NO NUMBER ENTERED                          
*                                                                               
*        FIND LAST SEQ NUMBER IN MINIO SET                                      
*              ACTION DISPLAY WILL SHOW THIS SEQ                                
*              ACTION ADD     WILL SHOW NEXT SEQ NUMBER                         
*                                                                               
         MVC   SVHDRELM,ELEMENT    INIT HEADER SAVEAREA                         
*                                                                               
         MVI   WIOSKLEN,WIOSKSQN-WIOSKEY  FILTER ON ANY STATUS ELEMENT          
*                                                                               
         GOTOR GETELM,DMCB,WIOSKEY  FIND FIRST OF SEQ                           
*                                                                               
VKFLSTLP DS    0H                  FIND LAST SEQ NUMBER                         
*                                                                               
         BNZ   VKFLSTDN            NO MORE SEQS                                 
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOSKCDE,WIOSKIDQ   DONE IF NOT STATUS ELEMENT                   
         BNE   VKFLSTDN                                                         
*                                                                               
         MVC   SVHDRELM,0(R6)      SAVE STATUS ELEMENT                          
*                                                                               
VKFLSTCN DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOSKEY NEXT ELEMENT                                 
*                                                                               
         B     VKFLSTLP                                                         
*                                                                               
VKFLSTDN DS    0H                                                               
*                                                                               
         LA    R6,SVHDRELM         POINT TO LAST HEADER FOUND                   
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING, USE NEXT AVAILABLE #              
         BE    *+8                                                              
         CLI   ACTNUM,ACTABADD                                                  
         BNE   VKFLSTD1                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,WIOSKSQN         GET LAST SEQ NUMBER                          
         AHI   R0,1                   BUMP SEQ NUMBER                           
         STC   R0,WIOSKSQN            SET IN ELEMENT KEY                        
*                                                                               
VKFLSTD1 DS    0H                  DISPLAY LAST SEQ                             
*                                                                               
         CLI   WIOSKSQN,0          MUST HAVE A NUMBER BY NOW                    
         BE    VKSTAT2E                                                         
*                                                                               
         MVC   QSSQN,WIOSKSQN      SAVE SEQ NUMBER                              
*                                                                               
VKFLSTX  DS    0H                                                               
*                                                                               
         B     VKSTAT30                                                         
*                                                                               
VKSSQN10 DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTDIS       IF ACTION DISPLAY                            
         BE    *+8                                                              
         CLI   ACTNUM,ACTSEL       IF ACTION SELECT                             
         BNE   VKSTAT05               CHECK PFKEY                               
*                                                                               
VKSTAPK  DS    0H                                                               
*                                                                               
         CLI   PFAID,8             IF PFKEY 8                                   
         BE    *+8                                                              
         CLI   PFAID,20            OR PFKEY 20 HIT                              
         BNE   *+8                                                              
         AHI   RF,1                   BUMP SEQ NUMBER                           
*                                                                               
         CLI   PFAID,7             IF PFKEY 7                                   
         BE    *+8                                                              
         CLI   PFAID,19            OR PFKEY 19 HIT                              
         BNE   *+8                                                              
         AHI   RF,1                   DECREMENT SEQ NUMBER                      
*                                                                               
         CHI   RF,0                IF TOP OF LIST FOUND                         
         BE    VKSTAT2E               NO MORE TO FIND                           
*                                                                               
VKSTAT05 DS    0H                                                               
*                                                                               
         STCM  RF,1,WIOSKSQN           SET SEQ NUMBER                           
*                                                                               
         MVI   WIOSKLEN,WIOSKSQN-WIOSKEY SET TO LOOK FOR ANY IN SEQ             
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT READ FOR ELEMENT                             
         BNE   VKSTATNF            ELEMENT NOT FOUND                            
*                                                                               
         CLI   ACTNUM,ACTADD       IF FOUND, ACTION CAN'T BE ADD                
         BE    *+8                                                              
         CLI   ACTNUM,ACTABADD                                                  
         BE    VKSTAT1E                                                         
*                                                                               
         L     RF,MINELEM          POINT TO FOUND ELEMENT                       
         MVC   SVHDRELM,0(RF)      SAVE FOUND DETAIL                            
*                                                                               
         B     VKSTAT30                                                         
*                                                                               
VKSTATNF DS    0H                  ELM NOT FOUND                                
*                                                                               
         CLI   ACTNUM,ACTADD          OKAY IF ACTION ADD                        
         BE    *+8                                                              
         CLI   ACTNUM,ACTABADD                                                  
         BE    VKSTAT30                                                         
*                                                                               
         CLI   ACTNUM,ACTDIS       ERROR IF NOT DISPLAYING                      
         BNE   VKSTAT2E                                                         
*                                                                               
         CLI   PFAID,8             ERROR IF NOT PFKEY DOWN                      
         BE    *+8                                                              
         CLI   PFAID,20                                                         
         BE    *+8                                                              
         CLI   PFAID,7             OR PFKEY UP                                  
         BE    *+8                                                              
         CLI   PFAID,19                                                         
         BNE   VKSTAT2E                                                         
*                                                                               
         B     VKSTAPK                                                          
*                                                                               
VKSTAT10 DS    0H                  NO SSQN PROVIDED                             
*                                  FIND NEXT AVAILABLE                          
         MVI   WIOSKLEN,WIOSKLEN-WIOSKEY  SET FOR COMPARE ON CODE ONLY          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,WIOSKSQN       SAVE CURRENT SEQ NUMBER                      
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT  FIND FIRST DETAIL                           
*                                                                               
VKSTATLP DS    0H                                                               
*                                                                               
         BNZ   VKSTATDN            END OF DETAILS FOUND                         
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         ICM   R0,1,WIOSKSQN       SAVE FOUND SEQ NUMBER                        
*                                                                               
VKSTATCN DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,ELEMENT FIND NEXT DETAIL                             
*                                                                               
         B     VKSTATLP                                                         
*                                                                               
VKSTATDN DS    0H                                                               
*                                                                               
         LA    R6,ELEMENT          RE-POINT TO ELEMENT BUILD AREA               
*                                                                               
         AHI   R0,1                BUMP SEQ NUMBER BY ONE                       
         STCM  R0,1,WIOSKSQN       SET AS NEW SEQ NUMBER                        
*                                                                               
VKSTAT30 DS    0H                                                               
*                                                                               
*        RE-DISPLAY STATUS SEQ NUMBER                                           
*                                                                               
         EDIT  WIOSKSQN,SSTSQN,0,ALIGN=LEFT                                     
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         MVI   FLDOLEN,L'SSTSQN    MAX OUTPUT                                   
*                                                                               
*        GENCON NEEDS A VALUE IN KEY                                            
*                                                                               
VKKEY10  DS    0H                                                               
*                                                                               
         LA    R4,WIOKEY           SET KEY AS MASTER WIO KEY                    
         USING WIOKEY,R4                                                        
*                                                                               
*        GENCON NEEDS A KEY THAT CAN BE FOUND                                   
*                                                                               
         MVC   WIOKELMK,=8X'FF'    SET FOR MASTER MINIO KEY                     
*                                                                               
         MVC   KEY,MINMKEY         SET MASTER PART OF KEY                       
*                                                                               
         CLC   SVIOKEY,QIOKEY     IF INSORD KEY HAS CHANGED                     
         BE    *+16                                                             
         XC    SVHDRELM,SVHDRELM      INIT HDR ELM SAVEAREA                     
         MVC   SVIOKEY,QIOKEY         UPDATE KEY SAVEAREA                       
*                                                                               
         CLI   ACTNUM,ACTSEND      IF ACTION SEND                               
         BE    *+8                                                              
         CLI   ACTNUM,ACTABDEL     OR ACTION ABDELETE                           
         BNE   *+8                                                              
         BRAS  RE,DR                  DISPLAY RECORD                            
*                                                                               
VKX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'PPWIO00 - PRINT NEW INSORD CONTROLLER - VKERR '                 
***********************************************************************         
*                                                                     *         
*        VALKEY ERROR MESSAGES                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKMEDER  LHI   RF,PPEFLDNE        MEDIA REQUIRED                                
         J     VKERR                                                            
*                                                                               
VKCLTER  LHI   RF,PPEFLDNE        CLIENT REQUIRED                               
         J     VKERR                                                            
*                                                                               
VKPERER  LHI   RF,PPEFLDNE        PERIOD REQUIRED                               
         J     VKERR                                                            
*                                                                               
VKPUBER  LHI   RF,PPEFLDNE        PUB    REQUIRED                               
         J     VKERR                                                            
*                                                                               
VKIO#ER  LHI   RF,PPEFLDNE        INSORD   REQUIRED                             
         J     VKERR                                                            
*                                                                               
VKSSQNER LHI   RF,PPEFLDNE        INSORD   REQUIRED                             
         J     VKERR                                                            
*                                                                               
VKRV#ER  LHI   RF,PPEFLDNE        INSORD   REQUIRED                             
         J     VKERR                                                            
*                                                                               
VKIO#1ER LHI   RF,PPEINVBG        INSORD   NUMBER TOO LARGE                     
         J     VKERR                                                            
*                                                                               
VKIO#2ER LHI   RF,PPEIO#NF        INSORD   NOT ON FILE                          
         J     VKERR                                                            
*                                                                               
VKDELER  LHI   RF,PPERECDL        RECORD IS DELETED                             
         J     VKERR                                                            
*                                                                               
VKDEL1ER LHI   RF,PPERECDL        RECORD IS DELETED                             
         J     VKERR1                                                           
*                                                                               
*        DETAIL ERROR MESSAGES TO BE FORMULATED                                 
*                                                                               
VKSTAT1E LHI RF,PPESTAFD           RECORD ALREADY ON FILE                       
         J     VKERR1                                                           
*                                                                               
VKSTAT2E LHI RF,PPESTANF           INSORD LINE ITEM NOT ON FILE                 
         J     VKERR1                                                           
*                                                                               
VKSTAT3E LHI RF,PPESTANF           INSORD LINE ITEM MUST BE NUMERIC             
         J     VKERR1                                                           
*                                                                               
VKERR    DS    0H                  INSORD RECORD DOES NOT EXIST                 
*                                    CLEAR SEQ # AND PERIOD                     
*                                                                               
         LR    R0,R2               SAVE FIELD POINTER                           
*                                                                               
         LA    R2,SSTSQNH          POINT TO SEQ NUMBER FIELD                    
         BRAS  RE,CLRFLD           CLEAR INSORD SERIAL NUMBER FIELD             
*                                                                               
         LA    R2,SSTPERH          POINT TO PERIOD FIELD                        
         BRAS  RE,CLRFLD           CLEAR INSORD PERIOD FIELD                    
*                                                                               
         LR    R2,R0               RESTORE FIELD POINTER                        
*                                                                               
VKERR1   DS    0H                                                               
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         XIT1                      DOWNLOADS AND STATUS RETURN                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPWIO00 - PRINT NEW INSORD CONTROLLER - ABADD'                  
***********************************************************************         
*                                                                     *         
*        ADD SEVERAL STATUSES TO THE FILE                             *         
*                                                                     *         
***********************************************************************         
*                                                                               
ABADD    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
*                                                                               
ABADDLP  DS    0H                                                               
*                                                                               
         BRAS  RE,VR               HANDLE STATUS ALREADY  ON SCREEN             
*                                                                               
ABADDCN  DS    0H                                                               
*                                                                               
         BRAS  RE,VK               PUT NEXT STATUS ON SCREEN                    
*                                                                               
         CLI   LNKSTSW,C'L'        LOOP IF NOT END OF LIST                      
         BNE   ABADDLP                                                          
*                                                                               
ABADDDN  DS    0H                                                               
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)                                                      
         USING LIOBD,R3               ESTABLISH LINKIO INTERFACE BLOCK          
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOACLO',LIOBD) CLOSE WORKER FILE                 
*                                                                               
ABADDX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPWIO00 - PRINT NEW INSORD CONTROLLER - FILLKEY'                
***********************************************************************         
*                                                                     *         
*        READ NEXT IOKEY AND FILL IN KEY FIELDS                       *         
*                                                                     *         
* LNKWIOKY  -  LONG FORM OF WEBIO KEY                                 *         
*        FILLS IN VARIOUS Q FIELDS                                    *         
*        OPENS MINIO SET                                              *         
*                                                                     *         
***********************************************************************         
*                                                                               
FILLKEY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
*                                                                               
         ICM   R3,15,NXTIOKYA      POINT TO NEXT IOKEY IN WKR FILE              
         BNZ   FSCR10                                                           
*                                                                               
         GOTOR GETINPUT,DMCB,LNKAIO   SAVE INPUT                                
*                                                                               
         LHI   R3,SVINPUT-SYSD                                                  
         LA    R3,SYSD(R3)            POINT TO SAVED INPUT                      
         MVI   LNKSTSW,C'1'           INDICATE FIRST TIME                       
*                                                                               
FSCR10   DS    0H                                                               
*                                                                               
FSCRLOOP DS    0H                                                               
*                                                                               
         USING WKRDATD,R3          ESTABLISH WORKER FILE ENTRY                  
*                                                                               
         OC    WKDTMPCD,WKDTMPCD   DONE AT END OF DATA                          
         BZ    FSCRDONE                                                         
*                                                                               
         CLC   WKDTMPCD,=AL2(D#IOLKEY)  MUST BE FOR IOKEY                       
         BNE   FSCRDONE                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WKDTRLEN       GET DATA LENGTH                              
         LR    R0,RF               SAVE ELEMENT LENGTH                          
         SHI   RF,WKDTHDLQ         SUBTRACT HEADER LENGTH                       
         BZ    FSCRIO#X              NO DATA                                    
*                                                                               
         CHI   RF,L'LNKWIOKY       ERROR IF TOO LONG                            
         BH    FSCRIO#X                                                         
*                                                                               
         XC    LNKWIOKY,LNKWIOKY   INIT SAVEAREA                                
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LNKWIOKY(0),WKDTDATA MOVE TEXT TO WORK FIELD                     
*                                                                               
         OC    LNKWIOKY,LNKWIOKY                                                
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE WIO KEY!                           
*                                                                               
         SR    R2,R2                                                            
         GOTOR PRSIO#,DMCB,(L'LNKWIOKY,LNKWIOKY)  BREAK OUT KEY                 
*                                                                               
*        SET MEDIA ON SCREEN                                                    
*                                                                               
         MVC   SSTMED,SPACES                                                    
         MVC   SSTMED(L'QMED),QMED                                              
         MVI   SSTMEDH+5,L'QMED                                                 
*                                                                               
*        SET CLIENT ON SCREEN                                                   
*                                                                               
         MVC   SSTCLT,SPACES                                                    
         MVC   SSTCLT(L'QCLT),QCLT                                              
         MVI   SSTCLTH+5,L'QCLT                                                 
*                                                                               
*        SET INSERTION ORDER # ON SCREEN                                        
*                                                                               
         MVC   SSTIO#,SPACES                                                    
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,QIO#IOYR       GET IO# YEAR                                 
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  SSTIO#(2),DUB                                                    
*                                                                               
         ICM   RF,7,QIO#IOSQ       GET IO# SEQ NUMBER                           
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
*                                                                               
         LA    RF,4                PRINT 4 DIGITS                               
         CP    DUB,=P'9999'        IF OVER 9999                                 
         BNH   *+8                                                              
         LA    RF,5                   PRINT 5 DIGITS                            
*                                                                               
         BCTR  RF,0                                                             
         SLL   RF,4                MOVE LENGTH TO LEFT NYBBLE                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  SSTIO#+2(0),DUB                                                  
*                                                                               
         SRL   RF,4                MOVE TO RIGHT NYBBLE                         
         AHI   RF,3                TRUE FIELD LENGTH                            
         STC   RF,SSTIO#H+5        YEAR+SQN                                     
*                                                                               
*        SET REVISION # ON SCREEN                                               
*                                                                               
         MVC   SSTREV#,SPACES                                                   
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,QREV#          GET REVISION #                               
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  SSTREV#(3),DUB                                                   
*                                                                               
         MVI   SSTREV#H+5,3        SET INPUT LENGTH                             
*                                                                               
FSCRIO#X DS    0H                                                               
*                                                                               
         AR    R3,R0               BUMP TO NEXT FIELD IN WKR FILE               
*                                                                               
*        SKIP IF HISTORY ID                                                     
*                                                                               
         CLC   WKDTMPCD,=AL2(D#HSTYID)  SKIP IF HISTID                          
         BNE   FSCRHSTX                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WKDTRLEN       GET DATA LENGTH                              
         LR    R0,RF               SAVE ELEMENT LENGTH                          
*                                                                               
         AR    R3,R0               BUMP TO NEXT FIELD IN WKR FILE               
*                                                                               
FSCRHSTX DS 0H                                                                  
*                                                                               
*        SET PUB ON SCREEN                                                      
*                                                                               
         CLC   WKDTMPCD,=AL2(D#PUBCOD)  MUST BE FOR PUB                         
         BNE   FSCRCONT                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WKDTRLEN       GET DATA LENGTH                              
         LR    R0,RF               SAVE ELEMENT LENGTH                          
         SHI   RF,WKDTHDLQ         SUBTRACT HEADER LENGTH                       
         BZ    FSCRPUBX              NO DATA                                    
*                                                                               
         CHI   RF,L'SSTPUB         ERROR IF TOO LONG                            
         BH    FSCRPUBX                                                         
*                                                                               
         XC    SSTPUB,SSTPUB                                                    
         STC   RF,SSTPUBH+5        SET INPUT LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SSTPUB(0),WKDTDATA  MOVE TEXT TO SCREEN FIELD                    
*                                                                               
FSCRPUBX DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTSTAT      DONE IF STATUS INQUIRY                       
         BE    FSCRCONT                                                         
*                                                                               
         AR    R3,R0               BUMP TO NEXT FIELD IN WKR FILE               
*                                                                               
*        SET STATUS ON SCREEN (MAY NOT BE PRESENT)                              
*                                                                               
         CLC   WKDTMPCD,=AL2(D#IORSTA)  MUST BE FOR STATUS                      
         BNE   FSCRLAST                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WKDTRLEN       GET DATA LENGTH                              
         LR    R0,RF               SAVE ELEMENT LENGTH                          
         SHI   RF,WKDTHDLQ         SUBTRACT HEADER LENGTH                       
         BZ    FSCRCONT              NO DATA                                    
*                                                                               
         CHI   RF,L'SSTSTA         ERROR IF TOO LONG                            
         BH    FSCRCONT                                                         
*                                                                               
         XC    SSTSTA,SSTSTA                                                    
         STC   RF,SSTSTAH+5        SET INPUT LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SSTSTA(0),WKDTDATA  MOVE TEXT TO SCREEN FIELD                    
*                                                                               
FSCRCONT DS    0H                                                               
*                                                                               
         AR    R3,R0               BUMP TO NEXT IO KEY                          
         ST    R3,NXTIOKYA         SAVE POINTER                                 
*                                                                               
         B     FILLKEYX                                                         
*                                                                               
FSCRDONE DS    0H                                                               
*                                                                               
         MVI   LNKSTSW,C'L'        SET TO LAST IO FOUND                         
*                                                                               
FSCRLAST DS    0H                                                               
*                                                                               
FILLKEYX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPWIO00 - PRINT NEW INSORD CONTROLLER - GETFLTS'                
***********************************************************************         
*                                                                     *         
*        READ NEXT SET OF FILTERS                                     *         
*                                                                     *         
* LNKAFLT   -  A(FIRST OF FILTER FIELDS)                              *         
*                                                                     *         
***********************************************************************         
*                                                                               
GETFLTS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
*                                                                               
*        INIT FILTERS                                                           
*                                                                               
         XC    QFMED,QFMED         MEDIA                                        
         XC    QFCLT,QFCLT         CLIENT                                       
         XC    QFPRD,QFPRD         PRODUCT                                      
         XC    QFPUB,QFPUB         PUB                                          
         XC    QFPER,QFPER         PERIOD                                       
*                                                                               
         ICM   R3,15,NXTFLTA       POINT TO NEXT SET OF FILTERS IN WRKR         
         BNZ   *+16                                                             
         LHI   R3,SVINPUT-SYSD                                                  
         LA    R3,SYSD(R3)            POINT TO SAVED INPUT                      
         MVI   LNKFLSW,C'1'           INDICATE FIRST TIME                       
*                                                                               
GFLTLOOP DS    0H                                                               
*                                                                               
         USING WKRDATD,R3          ESTABLISH WORKER FILE ENTRY                  
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,WKDTMPCD       GET MAP CODE                                 
         BZ    GFLTLAST            NO MAP CODE FOUND                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WKDTRLEN       GET DATA LENGTH                              
         LR    R0,RF               SAVE ELEMENT LENGTH                          
*                                                                               
         CHI   RE,D#EOR            DONE AT END OF RECORD                        
         BE    GFLTDONE                                                         
*                                                                               
         SHI   RF,WKDTHDLQ         SUBTRACT HEADER LENGTH                       
         BNP   GFLTCONT            SKIP IF NO DATA                              
*                                                                               
*        MEDIA CODE                                                             
*                                                                               
GFLTMED  DS    0H                                                               
*                                                                               
         CHI   RE,D#MEDCOD         MEDIA CODE?                                  
         BNE   GFLTMEDN                                                         
*                                                                               
         CHI   RF,L'QFMED          SKIP IF TOO LONG                             
         BH    GFLTMEDX                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   QFMED(0),WKDTDATA   MEDIA TO WORK FIELD                          
*                                                                               
         MVC   QMED,QFMED          FOR FOLLWING VALIDATION                      
*                                                                               
GFLTMEDX DS    0H                                                               
         B     GFLTCONT                                                         
*                                                                               
GFLTMEDN DS    0H                                                               
*                                                                               
*        CLIENT CODE                                                            
*                                                                               
GFLTCLT  DS    0H                                                               
*                                                                               
         CHI   RE,D#CLTCOD         CLIENT CODE?                                 
         BNE   GFLTCLTN                                                         
*                                                                               
         CHI   RF,L'QFCLT          ERROR IF TOO LONG                            
         BH    GFLTCONT                                                         
*                                                                               
         MVC   QFCLT,SPACES        INIT THE FIELD                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   QFCLT(0),WKDTDATA   CLIENT TO WORK FIELD                         
*                                                                               
         MVC   QCLT,QFCLT          FOR FOLLWING VALIDATION                      
*                                                                               
GFLTCLTX DS    0H                                                               
         B     GFLTCONT                                                         
*                                                                               
GFLTCLTN DS    0H                                                               
*                                                                               
*        PRODUCT CODE                                                           
*                                                                               
GFLTPRD  DS    0H                                                               
*                                                                               
         CHI   RE,D#PRDCOD         PRODUCT CODE?                                
         BNE   GFLTPRDN                                                         
*                                                                               
         CHI   RF,L'QFPRD          ERROR IF TOO LONG                            
         BH    GFLTCONT                                                         
*                                                                               
         MVC   QFPRD,SPACES        INIT THE FIELD                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   QFPRD(0),WKDTDATA   PRODUCT TO WORK FIELD                        
*                                                                               
         MVC   QPRD,QFPRD          FOR FOLLWING VALIDATION                      
*                                                                               
GFLTPRDX DS    0H                                                               
         B     GFLTCONT                                                         
*                                                                               
GFLTPRDN DS    0H                                                               
*                                                                               
*        PUB     CODE                                                           
*                                                                               
GFLTPUB  DS    0H                                                               
*                                                                               
         CHI   RE,D#PUBCOD         PUB CODE?                                    
         BNE   GFLTPUBN                                                         
*                                                                               
         CHI   RF,L'SSTPUB         ERROR IF TOO LONG                            
         BH    GFLTCONT                                                         
*                                                                               
         LA    R2,SSTPUBH                                                       
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         STC   RF,SSTPUBH+5        SET FIELD LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SSTPUB(0),WKDTDATA   PRODUCT TO SCREEN FIELD                     
*                                                                               
         GOTOR VALPUB              VALIDATE PUB                                 
*                                                                               
         MVC   QFPUB,QPUB          SAVE BASE PUB CODE                           
*                                                                               
GFLTPUBX DS    0H                                                               
         B     GFLTCONT                                                         
*                                                                               
GFLTPUBN DS    0H                                                               
*                                                                               
*        PERIOD  CODE                                                           
*                                                                               
GFLTPER  DS    0H                                                               
*                                                                               
         CHI   RE,D#STEND          PERIOD CODE?                                 
         BNE   GFLTPERN                                                         
*                                                                               
         LA    R2,SSTPERH                                                       
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         CHI   RF,L'SSTPER         ERROR IF TOO LONG                            
         BH    GFLTCONT                                                         
*                                                                               
         STC   RF,SSTPERH+5        SET FIELD LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SSTPER(0),WKDTDATA   PRODUCT TO SCREEN FIELD                     
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    SSTPER(0),SPACES     MAKE UPPERCASE                              
*                                                                               
         GOTOR VALPER              VALIDATE PERIOD                              
*                                                                               
         MVC   QFPER,QPER          SAVE PERIOD                                  
*                                                                               
         XC    QPER,QPER           RESET PERIOD                                 
*                                                                               
GFLTPERX DS    0H                                                               
         B     GFLTCONT                                                         
*                                                                               
GFLTPERN DS    0H                                                               
*                                                                               
GFLTCONT DS    0H                                                               
*                                                                               
         AR    R3,R0               BUMP TO NEXT IO KEY                          
*                                                                               
         B     GFLTLOOP                                                         
*                                                                               
GFLTDONE DS    0H                                                               
*                                                                               
         AR    R3,R0               BUMP TO NEXT IO KEY                          
         ST    R3,NXTFLTA          SAVE POINTER                                 
*                                                                               
         B     GETFLTX                                                          
*                                                                               
GFLTLAST DS    0H                                                               
*                                                                               
         MVI   LNKFLSW,C'L'        SET TO LAST IO FLTR FOUND                    
*                                                                               
GETFLTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPWIO00 - PRINT NEW INSORD CONTROLLER - GETINPUT'               
***********************************************************************         
*                                                                     *         
*        SAVE LINKIO INPUT                                            *         
*                                                                     *         
***********************************************************************         
*                                                                               
GETINPUT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
*                                                                               
         L     R4,0(R1)            SACE A(INPUT)                                
*                                                                               
         LA    R3,0(R4)            POINT TO START OF INPUT                      
         LHI   R1,SVINPUT-SYSD                                                  
         LA    R1,SYSD(R1)         POINT TO SAVED INPUT                         
         SR    RF,RF                                                            
         SR    RE,RE                                                            
*                                                                               
GINPLOOP DS    0H                                                               
*                                                                               
         USING WKRDATD,R3          ESTABLISH WORKER FILE ENTRY                  
*                                                                               
         ICM   RE,3,WKDTMPCD       GET MAP CODE                                 
         BZ    GINPDONE            END OF INPUT                                 
*                                                                               
         ICM   RF,3,WKDTRLEN       GET DATA LENGTH                              
         BCTR  RF,0                DECREMENT FO EXECUTE                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R3)       MOVE TO WORK AREA                            
*                                                                               
GINPCONT DS    0H                                                               
*                                                                               
         LA    R1,1(RF,R1)         NEXT SAVEAREA                                
         LA    R3,1(RF,R3)         NEXT INPUT ELEMENT                           
*                                                                               
         B     GINPLOOP                                                         
*                                                                               
GINPDONE DS    0H                                                               
*                                                                               
*        NOW THAT WE HAVE THE INPUT                                             
*              MAKE SURE WE HAVE READ TO THE END OF THE FILE                    
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)                                                      
         USING LIOBD,R3            ESTABLISH LINKIO INTERFACE BLOCK             
*                                                                               
         TM    LIOBFLG2,LIOBFEOF   TEST FOR EOF ALREADY FOUND                   
         BO    GINP10                                                           
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAGET',LIOBD)                                   
*                                                                               
         TM    LIOBFLG2,LIOBFEOF   MUST BE EOF                                  
         BO    *+6                                                              
         DC    H'0'                ALL DATA FIELDS SHOULD BE PROC'D             
*                                                                               
GINP10   DS    0H                                                               
*                                                                               
GETINPX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPWIO00 - PRINT NEW INSORD CONTROLLER - TRDATTP'                
***********************************************************************         
*                                                                     *         
*        TRANSLATE DATA   TYPES INTO BITS                             *         
*                                                                     *         
*NTRY                                                                 *         
* LNKDATTP  -  INCOMING LIST OF DATA   TYPES WANTED                   *         
*                                                                     *         
*EXIT       WRKDATTP - LIST OF TYPES TRANSLATED TO BITS               *         
*                                                                     *         
***********************************************************************         
*                                                                               
         DS    0D                                                               
TRDATTP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
*                                                                               
         LA    R1,LNKDATTP         POINT TO DATA TYPES LIST                     
         LA    R0,L'LNKDATTP       MAX NUMBER OF TYPES                          
         XC    WRKDATTP,WRKDATTP   INIT TYPES ACCUMULATOR                       
*                                                                               
TRDLOOP  DS    0H                                                               
*                                                                               
         CLI   0(R1),C' '          DONE ON END OF LIST                          
         BNH   TRDDONE                                                          
*                                                                               
         LA    R2,DTYPLST          POINT TO LIST OF DATA TYPES                  
*                                                                               
TRDLSTLP DS    0H                                                               
*                                                                               
         CLI   0(R2),X'FF'         DONE AT END OF LIST                          
         BE    TRDLSTDN                                                         
*                                                                               
         CLC   0(1,R1),0(R2)       MATCH INPUT TO LIST ITEM                     
         BE    TRDLSTFD                                                         
*                                                                               
TRDLSTCN DS    0H                                                               
         LA    R2,DTYPLSTL(R2)     BUMP TO NEXT LIST ENTRY                      
         B     TRDLSTLP                                                         
*                                                                               
TRDLSTFD DS    0H                  MATCH FOUND IN LIST                          
*                                                                               
         OC    WRKDATTP,1(R2)      ADD BIT CONFIGURATION TO TYPES               
*                                                                               
TRDLSTDN DS    0H                                                               
*                                                                               
TRDCONT  DS    0H                                                               
         LA    R1,1(R1)            NEXT ITEM IN LIST                            
         BCT   R0,TRDLOOP                                                       
*                                                                               
TRDDONE  DS    0H                                                               
*                                                                               
TRDATTPX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
DTYPLST  DS    0H                  LIST TO DATA TYPES TO DOWN LOAD              
         DC    AL1(LNKALLDQ,WRKALLDQ) ALL TYPES                                 
DTYPLSTL EQU   *-DTYPLST           LENGTH OF ENTRY IN LIST                      
         DC    AL1(LNKHDRDQ,WRKHDRDQ) HEADER DATA                               
         DC    AL1(LNKEMLDQ,WRKEMLDQ) E-MAIL DATA                               
         DC    AL1(LNKVDRDQ,WRKVDRDQ) VENDOR DATA                               
         DC    AL1(LNKURLDQ,WRKURLDQ) URLS   DATA                               
         DC    AL1(LNKSTADQ,WRKSTADQ) STATUS DATA                               
         DC    AL1(LNKBUYDQ,WRKBUYDQ) BUY    DATA                               
         DC    X'FF'               EOT                                          
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E05 - INIT LINKIO BASE ADDRESSES - INILIOB'                  
***********************************************************************         
*                                                                     *         
*        INITIALIZE LIOB BASE ADDRESSES                               *         
*                                                                     *         
***********************************************************************         
*                                                                               
INILIOB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)                                                      
         USING LIOBD,R3            ESTABLISH LINKIO INTERFACE BLOCK             
*                                                                               
         ST    R9,LIOBASB1         SET WORK BASE ADDRESS                        
         ST    RA,LIOBASB2         SET SCREEN BASE ADDRESS                      
*                                                                               
INILIOBX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
                                                                                
         TITLE 'T41E05 - GLOBAL CONSTANTS - GLOBALS'                            
***********************************************************************         
*                                                                     *         
         TITLE 'T41E30 - STATUS MAINT/LIST - VKL'                               
***********************************************************************         
*                                                                     *         
*        VALIDATE KEY FIELDS - LIST SCREEN                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKL      NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         MVI   CHGSWTCH,0          INITIALIZE CHANGE SWITCH                     
*                                                                               
*        SET NUMBER OF LINES AVAILABLE FOR LIST                                 
*                                                                               
         MVI   NLISTS,(LSTLINLH-LSTLIN1H)/(LSTLIN2H-LSTLIN1H)+1                 
*                                                                               
*        VALIDATE MEDIA                                                         
*                                                                               
         LA    R2,LSTMEDH          POINT TO MEDIA FIELD                         
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         GOTOR VALMED              VALIDATE MEDIA                               
*                                                                               
*        VALIDATE CLIENT                                                        
*                                                                               
         LA    R2,LSTCLTH          POINT TO CLIENT FIELD                        
*                                                                               
         GOTOR VALCLT              VALIDATE CLIENT                              
*                                                                               
*        VALIDATE PUB                                                           
*                                                                               
         LA    R2,LSTPUBH          POINT TO PUB FIELD                           
*                                                                               
         GOTOR VALPUB              VALIDATE PUB                                 
*                                                                               
         EJECT                                                                  
*                                                                               
*        VALIDATE INSORD NUMBER                                                 
*                                                                               
         LA    R2,LSTIO#H          POINT TO INSORD FIELD                        
*                                                                               
         GOTOR VALIO#                                                           
*                                                                               
*        FIND INSORD MASTER MINIO KEY                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH INSORD KEY                         
         USING WIOKEY,R4                                                        
*                                                                               
         MVC   WIOKAGY,QAGY        SET AGENCY                                   
         MVC   WIOKMED,QMED        SET MEDIA                                    
         MVI   WIOKRCD,WIOKRCDQ    SET RECORD CODE                              
         MVC   WIOKCLT,QCLT        SET CLIENT                                   
         MVC   WIOKPUB,QPUB        SET PUB                                      
         MVC   WIOKIO#,QIO#        SET INSORD  NUMBER                           
         MVC   WIOKRV#,QREV#       SET REVSION NUMBER                           
*                                                                               
         GOTOR HIGH                READ PRTDIR FOR KEY                          
*                                                                               
         CLC   WIOKEY(WIOKELMK-WIOKEY),KEYSAVE TEST IF KEY FOUND                
         BNE   VKLIO2ER            MUST FIND KEY                                
*                                                                               
*        READ IN INSORD MASTER RECORD                                           
*                                                                               
         MVC   QIOKEY,WIOKEY       SAVE MASTER KEY                              
         MVC   QIO#,WIOKIO#                                                     
         MVC   QREV#,WIOKRV#                                                    
*                                                                               
         LA    R2,LSTIO#H                                                       
*                                                                               
*        OPEN MINIO SET                                                         
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH INSORD MASTER KEY                  
         USING WIOKEY,R4                                                        
*                                                                               
         MVC   WIOKEY,KEY          SET MASTER KEY                               
*                                                                               
         MVI   MINDELSW,C'Y'       READ DELETED MINIO SETS                      
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0            NO ERRORS TOLERATED                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        FIND HEADER ELEMENT AND DISPLAY PERIOD                                 
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR ELEMENT WORKAREA                       
*                                                                               
         LA    R6,ELEMENT          BUILD HEADER ELEMENT KEY                     
         USING WIOHKEY,R6                                                       
*                                                                               
         MVI   WIOHKCDE,WIOHKIDQ   SET HEADER ELM CODE                          
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT READ FOR ELEMENT                             
         BE    *+6                 MUST FIND ELEMENT                            
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         LA    R2,LSTPERH          POINT TO PERIOD FIELD                        
*                                                                               
         GOTOR DISPER,DMCB,WIOHSTRT   DISPLAY PERIOD                            
*                                                                               
*        VALIDATE RUN PERIOD                                                    
*                                                                               
VKRPER   DS    0H                                                               
*                                                                               
         XC    BSTART,BSTART       CLEAR BINARY START                           
         XC    BEND,BEND           CLEAR BINARY END                             
*                                                                               
         LA    R2,LSTPERH          POINT TO RUN PERIOD                          
*                                                                               
         GOTOR VALPER              VALIDATE AS A PERIOD                         
*                                                                               
         BRAS  RE,CLRFLD           CLEAR RUN PERIOD FIELD                       
*                                                                               
         OC    BSTART,BSTART       SKIP IF NO PERIOD FILTER                     
         BZ    VKRPERX                                                          
*                                                                               
         GOTOR DISPER,DMCB,BSTART     DISPLAY PERIOD                            
*                                                                               
VKRPERX  DS    0H                                                               
*                                                                               
         CLC   SVIOKEY,QIOKEY     IF INSORD KEY HAS CHANGED                     
         BE    *+16                                                             
         XC    SVHDRELM,SVHDRELM      INIT STATUS ELM SAVEAREA                  
         MVC   SVIOKEY,QIOKEY        UPDATE KEY SAVEAREA                        
*                                                                               
VKLX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPWIO00 - PRINT NEW INSORD CONTROLLER - VKLERR '                
***********************************************************************         
*                                                                     *         
*        VALKEY ERROR MESSAGES                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKLMEDER LHI   RF,PPEFLDNE        MEDIA REQUIRED                                
         J     VKLERR                                                           
*                                                                               
VKLCLTER LHI   RF,PPEFLDNE        CLIENT REQUIRED                               
         J     VKLERR                                                           
*                                                                               
VKLPUBER LHI   RF,PPEFLDNE        PUB    REQUIRED                               
         J     VKLERR                                                           
*                                                                               
VKLIVER LHI    RF,PPEFLDNE        INSORD   REQUIRED                             
         J     VKLERR                                                           
*                                                                               
VKLIV1ER LHI   RF,PPEINVBG        INSORD   NUMBER TOO LARGE                     
         J     VKLERR                                                           
*                                                                               
VKLIO2ER LHI   RF,PPEIO#NF        INSORD   NOT ON FILE                          
         J     VKLERR                                                           
*                                                                               
VKLDELER LHI   RF,PPERECDL        RECORD IS DELETED                             
         J     VKLERR                                                           
*                                                                               
VKLDEL1ER LHI  RF,PPERECDL        RECORD IS DELETED                             
         J     VKLERR1                                                          
*                                                                               
*        DETAIL ERROR MESSAGES TO BE FORMULATED                                 
*                                                                               
VKLSTAT1E LHI RF,PPESTAFD          RECORD ALREADY ON FILE                       
         J     VKLERR1                                                          
*                                                                               
VKLSTAT2E LHI RF,PPESTANF          INSORD LINE ITEM NOT ON FILE                 
         J     VKLERR1                                                          
*                                                                               
VKLSTAT3E LHI RF,PPESTANF          INSORD LINE ITEM MUST BE NUMERIC             
         J     VKLERR1                                                          
*                                                                               
VKLERR   DS    0H                  INSORD RECORD DOES NOT EXIST                 
*                                                                               
VKLERR1  DS    0H                                                               
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         XIT1                      DOWNLOADS AND STATUS RETURN                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPWIO00 - PRINT NEW INSORD CONTROLLER - DISTIM'                 
***********************************************************************         
*                                                                     *         
*        DISPLAYS BINARY XL3 TIME AS HH:MM:SS                         *         
*                                                                     *         
*NTRY   R2==>   FIELD ON SCREEN                                       *         
*       P0      A(BINARY TIME FIELD)                                  *         
*                                                                     *         
*EXIT           TIME AS HH:MM:SS                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DISTIM   NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH FIELD ON SCREEN                    
*                                                                               
         L     R3,0(R1)            POINT TO TIME                                
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         IC    RF,0(R3)            GET HOURS                                    
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  FLDDATA(2),DUB                                                   
         MVI   FLDDATA+2,C':'                                                   
*                                                                               
         IC    RF,1(R3)            GET MINUTES                                  
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  FLDDATA+3(2),DUB                                                 
         MVI   FLDDATA+5,C':'                                                   
*                                                                               
         IC    RF,2(R3)            GET SECONDS                                  
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  FLDDATA+6(2),DUB                                                 
*                                                                               
DISTIMX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPWIO00 - PRINT NEW INSORD CONTROLLER - CLRFLD'                 
***********************************************************************         
*                                                                     *         
*        CLEARS A FIELD ON SCREEN AND FORCES RE-TRANSMITTAL           *         
*                                                                     *         
*NTRY   R2==>   FIELD ON SCREEN                                       *         
*                                                                     *         
*EXIT    FIELD CLEARED TO NULLS                                       *         
*        FIELD SET TO BE RE-TRANSMITTED                               *         
*        OUTPUT DATA LENGTH SET TO MAXIMUM                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CLRFLD   NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH FIELD ON SCREEN                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           GET TOTAL LENGTH OF FIELD                    
         AHI   RF,-(FLDDATA-FLDHDRD)  DECREMENT BY HEADER LENGTH                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENDED HEADER               
         BNO   *+8                                                              
         AHI   RF,-8                  DECREMENT BY EXTENDED SST LENGTH          
*                                                                               
         STC   RF,FLDOLEN          SET MAX OUTPUT LENGTH                        
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    FLDDATA(0),FLDDATA  CLEAR FIELD                                  
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
CLRFLDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPWIO00 - PRINT NEW INSORD CONTROLLER - SETLEN'                 
***********************************************************************         
*                                                                     *         
*        SETS TRUE INPUT LENGTH                                       *         
*                                                                     *         
*NTRY   R2==>   FIELD ON SCREEN                                       *         
*                                                                     *         
*EXIT    LENGTH OF ACTUAL LENGTH SET IN FIELD HEADER                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SETLEN   NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH FIELD ON SCREEN                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           GET TOTAL LENGTH OF FIELD                    
         AHI   RF,-(FLDDATA-FLDHDRD)  DECREMENT BY HEADER LENGTH                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENDED HEADER               
         BNO   *+8                                                              
         AHI   RF,-8                  DECREMENT BY EXTENDED SST LENGTH          
*                                                                               
         LA    R1,FLDDATA-1(RF)    POINT TO LAST BYTE OF INPUT                  
*                                                                               
SETLENLP DS    0H                  FIND LAST NON-BLANK IN FIELD                 
*                                                                               
         CLI   0(R1),C' '          DONE IF NOT BLANK                            
         BH    SETLENDN                                                         
*                                                                               
SETLENCN DS    0H                                                               
*                                                                               
         BCTR  R1,0                BACK UP A BYTE                               
         BCT   RF,SETLENLP                                                      
*                                                                               
SETLENDN DS    0H                                                               
*                                                                               
         STC   RF,FLDILEN          SET INPUT LENGTH                             
*                                                                               
SETLENX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPWIO30 - PRINT NEW STATUSS - BUMP'                             
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
         TITLE 'T41E30 - STATUS MAINT/LIST - LR'                                
***********************************************************************         
*                                                                     *         
*        BUILD LIST OF STATUSS                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LR       NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         OI    GLSTSTAT,RETEXTRA   1 MORE LINE THAN SCREEN HOLDS                
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         XC    MINMKEY,MINMKEY     INIT MASTER MINIO KEY                        
         MVC   MINMKEY(WIOKELMK-WIOKEY),QIOKEY SET MASTER KEY                   
*                                                                               
         MVI   MINDELSW,C'Y'       READ DELETED MINIO SETS                      
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
         CLI   MINERR,0            MUST FIND THE MINIO SET                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    MINSTAT,MINDELQ     SKIP IF DELETED                              
         BO    LRX                                                              
*                                                                               
         XC    ELEMENT,ELEMENT     INIT WORKAREA                                
         LA    R6,ELEMENT                                                       
         USING WIOSTATD,R6         ESTABLISH STATUS ELEMENT                     
*                                                                               
         MVC   WIOSKEY,SVLSTKEY    LAST USED ELEMENT KEY                        
*                                                                               
         CLI   LRLASTSW,C'Y'       IF END OF LIST LAST TIME                     
         BNE   *+14                                                             
         MVI   LRLASTSW,0             CLEAR SWITCH                              
         XC    ELEMENT,ELEMENT        CLEAR LAST KEY                            
*                                                                               
         OC    WIOSKEY,WIOSKEY     SKIP IF PRIOR KEY KNOWN                      
         BNZ   LRKEY10                                                          
*                                                                               
         MVI   WIOSKCDE,WIOSKIDQ   SET STATUS ELEMENT ID                        
         XC    WIOSKSQN,WIOSKSQN   SET FOR FIRST SEQ                            
*                                                                               
LRKEY10  DS    0H                                                               
*                                                                               
         MVI   WIOSKLEN,WIOSKSQN-WIOSKEY  MATCH ON STATUS ID                    
*                                                                               
         LA    R3,LISTAR           ESTABLISH LIST LINE                          
         USING LISTLIND,R3                                                      
*                                                                               
         LA    R5,SVLSTSST         POINT TO KEYS SAVEAREA                       
         XC    SVLSTSST(SVLSTLNQ),SVLSTSST  CLEAR TABLE                         
*                                                                               
         GOTO1 GETELM,DMCB,ELEMENT FIND FIRST ELEMENT                           
         BNZ   LRKEYDN             DONE AT END OF ELEMENTS                      
*                                                                               
LRKEYLP  DS    0H                                                               
*                                                                               
         XC    LISTLIN,LISTLIN     INIT PRINT AREA                              
*                                                                               
         L     R1,MINELEM          POINT TO FOUND ELEMENT                       
         MVC   ELEMENT,0(R1)       SAVE STATUS ELEMENT                          
*                                                                               
         CLI   WIOSKCDE,WIOSKIDQ   DONE IF NOT A STATUS ELEMENT                 
         BNE   LRKEYDN                                                          
*                                                                               
         MVC   SVLSTKEY,WIOSKEY    SAVE FOUND EKY                               
*                                                                               
*        DISPLAY SEQ NUMBER                                                     
*                                                                               
         EDIT  WIOSKSQN,LSSSQN,0,ALIGN=LEFT                                     
*                                                                               
*        DISPLAY SEQ STATUS                                                     
*                                                                               
LRSSTA   DS    0H                                                               
*                                                                               
         L     R2,=A(STATTB)        POINT TO STATUS TABLE                       
         A     R2,RELO20                                                        
*                                                                               
LRSTALP  DS    0H                                                               
*                                                                               
         CLI   0(R2),X'FF'         CHECK FOR END OF TABLE                       
         BE    LRSTAX              UNKNOWN STATUS                               
*                                                                               
         CLC   WIOSSTAT,0(R2)      FIND STATUS IN TABLE                         
         BE    LRSTAFD                                                          
*                                                                               
LRSTACN  DS    0H                                                               
*                                                                               
         LA    R2,STATTBL(R2)      BUMP TO NEXT STATUS                          
         B     LRSTALP                                                          
*                                                                               
LRSTAFD  DS    0H                                                               
*                                                                               
*        DISPLAY SEQ STATUS DATE AND TIME                                       
*                                                                               
         MVC   LSSTA,1(R2)         DISPLAY STATUS                               
*                                                                               
LRSDTE   DS    0H                                                               
*                                                                               
         GOTOR DATCON,DMCB,(3,WIOSDATE),(17,LSSDTE) DISPLAY DATE                
*                                                                               
         LA    R2,FLDH             POINT TO DUMMY FIELD                         
         GOTOR DISTIM,DMCB,WIOSTIME        DISPLAY TIME                         
         MVC   LSSTIM,FLD                                                       
*                                                                               
*        USE PID TO GET NAME                                                    
*                                                                               
         SR    R2,R2               NOT PRINTING IN STAND ALONE FIELD            
*                                                                               
         GOTOR TRNPID,DMCB,WIOSPID,(L'LSSNAM,LSSNAM)     PRINT NAME             
*                                                                               
         MVC   0(L'SVLSTSST,R5),WIOSKEY  SAVE STATUS KEY                        
         LA    R5,L'SVLSTSST(R5)   BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
         MVC   KEY,MINMKEY         RESET FILE POINTER                           
         GOTOR HIGH                                                             
*                                                                               
         MVC   DMDSKADD,KEY+27                                                  
*                                                                               
         GOTOR LISTMON             PASS BACK TO GENCON                          
*                                                                               
LRSTAX   DS    0H                                                               
*                                                                               
LRKEYCN  DS    0H                                                               
*                                                                               
         GOTO1 NXTELM,DMCB,ELEMENT    FIND NEXT  ELEMENT                        
*                                                                               
         B     LRKEYLP                                                          
*                                                                               
LRKEYDN  DS    0H                                                               
*                                                                               
         MVI   LRLASTSW,C'Y'       INDICATE END OF DETAILS                      
         XC    ELEMENT,ELEMENT     CLEAR WORKAREA                               
         XC    SVLSTKEY,SVLSTKEY   CLEAR SAVEAREA                               
*                                                                               
LRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
STATTB   DS    0D                  STATUS TRANSLATION TABLE                     
         DC    AL1(WIOSGENQ),CL16'GENERATED' DEFAULT                            
STATTBL  EQU   *-STATTB            TABLE ENTRY LENGTH                           
         DC    AL1(WIOSAPPQ),CL16'APPROVED '                                    
         DC    AL1(WIOSDLVQ),CL16'DELIVERED'                                    
         DC    AL1(WIOSREJQ),CL16'REJECTED '                                    
         DC    AL1(WIOSRSTQ),CL16'RESENT  '                                     
         DC    AL1(WIOSRSTQ),CL16'RE-SENT '                                     
         DC    AL1(WIOSSNTQ),CL16'SENT '                                        
         DC    AL1(WIOSUDLQ),CL16'UNDELIVERED'                                  
         DC    AL1(WIOSEXPQ),CL16'TIME EXPIRED'                                 
         DC    AL1(WIOSACCQ),CL16'ACCESSED    '                                 
         DC    AL1(0),CL16'GENERATED'      DEFAULT                              
         DC    XL1'FF'             EOT                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST - VR'                                
***********************************************************************         
*                                                                     *         
*        VALIDATE STATUS FIELDS                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VR       NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
*        IF ACTION ABDELETE                                                     
*              DELETE STATUS NUMBER                                             
*                                                                               
         CLI   ACTNUM,ACTABDEL     IF ACTION ABDELETE                           
         BNE   *+12                                                             
         BRAS  RE,DL                  DELETE LINE ITEM                          
         B     VRX                    ALL DONE                                  
*                                                                               
         CLI   ACTNUM,ACTHIST      IF ACTION DOWNLOAD                           
         BNE   *+12                                                             
         BRAS  RE,DWNLOAD                                                       
         B     VRX                                                              
*                                                                               
*        STATUS INQUIRYS  ARE SPECIAL                                           
*                                                                               
VRST     DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTSTAT      IF ACTION STATUS                             
         BNE   VRSTN                                                            
*                                                                               
         CLI   DDLNKSW,C'Y'        IF NOT IN A LINK CALL                        
         BE    *+12                                                             
         BRAS  RE,ST                  PROCESS STATUS                            
         B     VRX                                                              
*                                                                               
*        HANDLE LIST OF IOKEYS                                                  
*                                                                               
VRSTLKLP DS    0H                                                               
*                                                                               
         CLI   LNKSTSW,C'1'        IF FIRST TIME                                
         BNE   VRST1STN                                                         
*                                                                               
         MVI   LNKSTSW,C'2'           RESET SWITCH (VK ALREADY DONE)            
         B     VRST1STX                                                         
*                                                                               
VRST1STN DS    0H                  ELSE                                         
*                                                                               
         BRAS  RE,VK                  NEXT IO TO KEY AND VALIDATE               
*                                                                               
VRST1STX DS    0H                                                               
*                                                                               
         CLI   LNKSTSW,C'L'        EXIT IF END OF LIST FOUND                    
         BE    VRSTLKDN                                                         
*                                                                               
         BRAS  RE,ST               RETURN STATUS DATA                           
*                                                                               
*        IF WE WANT ALL REVISIONS, BUMP TO NEXT ONE                             
*                                                                               
         CLI   LNKREQTP,LNKONERQ   DONE IF SINGLE IO WANTED                     
         BE    VRSTDONE                                                         
*                                                                               
VRSTLOOP DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,QREV#            BUMP REVISION NUMBER                         
         AHI   RF,1                                                             
         STC   RF,QREV#                                                         
*                                                                               
         MVC   KEY,QIOKEY          COPY CURRENT KEY                             
         LA    R4,KEY              ESTABLISH WIOKEY                             
         USING WIOKEY,R4                                                        
*                                                                               
         STC   RF,WIOKRV#          UPDATE REVISION #                            
*                                                                               
         GOTOR HIGH                READ NEXT REVISION IO                        
*                                                                               
VRST1LP  DS    0H                                                               
*                                                                               
         CLC   WIOKELMK,=7X'FF'    OKAY IF MASTER KEY                           
         BE    VRST1DN                                                          
*                                                                               
VRST1CN  DS    0H                                                               
*                                                                               
         GOTOR SEQ                 READ NEXT POINTER                            
*                                                                               
         B     VRST1LP                                                          
*                                                                               
VRST1DN  DS    0H                                                               
*                                                                               
         CLC   WIOKEY(WIOKRV#-WIOKEY),KEYSAVE DONE IF NEW IO                    
         BNE   VRSTDONE                                                         
*                                                                               
         MVC   QIOKEY,WIOKEY       UPDATE IOKEY                                 
         MVC   QREV#,WIOKRV#       UPDATE REVISION NUMBER                       
*                                                                               
         LA    R2,SSTIO#H          POINT TO IO# FIELD                           
         GOTOR DISIO#              UPDATE SCREEN                                
*                                                                               
         MVC   LNKWIOKY,QIO#EXP    SAVE IO# EXPANSION                           
*                                                                               
         BRAS  RE,ST               PUT STATUS DATA                              
*                                                                               
VRSTCONT DS    0H                                                               
*                                                                               
         B     VRSTLOOP                                                         
*                                                                               
VRSTDONE DS    0H                                                               
*                                                                               
VRSTLKCN DS    0H                                                               
         B     VRSTLKLP            GET NEXT IO KEY                              
*                                                                               
VRSTLKDN DS    0H                                                               
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)                                                      
         USING LIOBD,R3               ESTABLISH LINKIO INTERFACE BLOCK          
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOACLO',LIOBD) CLOSE WORKER FILE                 
*                                                                               
VRSTX    DS    0H                                                               
         B     VRX                    ALL DONE                                  
*                                                                               
*        NON-DOWNLOAD ACTIONS                                                   
*                                                                               
VRSTN    DS    0H                                                               
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         XC    MINMKEY,MINMKEY     INIT MASTER MINIO KEY                        
         MVC   MINMKEY(WIOKELMK-WIOKEY),QIOKEY SET MASTER KEY                   
*                                                                               
         MVI   MINDELSW,C'Y'       READ DELETED MINIO SETS                      
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST - VRSTAT'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE STATUS                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRSTAT   DS    0H                                                               
*                                                                               
         LA    R2,SSTSTAH          POINT TO STATUS FIELD                        
*                                                                               
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         GOTOR GETFLD              READ IN FIELD (REQUIRED)                     
*                                                                               
         GOTOR VALSTA              VALIDATE STATUS                              
*                                                                               
         OI    SVACH3,WIOASADD     STATUS ADDED                                 
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
VRSTATX  DS    0H                                                               
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST - VRSQN'                             
***********************************************************************         
*                                                                     *         
*        ROUTINE TO FIND NEXT STATUS NUMBER                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRSQN  DS      0H                                                               
*                                                                               
*        COLLECT STATUS ELMS IN A TABLE                                         
*                                                                               
         XC    SVSTAELM,SVSTAELM   INIT STATUS ELM SAVEAREA                     
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT                                                       
         USING WIOSTATD,R6         ESTABLISH STATUS ELEMENT                     
*                                                                               
         MVI   WIOSKCDE,WIOSKIDQ   SET ELEMENT CODE                             
         MVI   WIOSKLEN,2          SET TO FIND STATUS ELEMENT                   
*                                                                               
         GOTOR GETELM,DMCB,WIOSKEY FIND ELEMENT                                 
*                                                                               
VRSQNLP  DS    0H                                                               
*                                                                               
         BNZ   VRSQNDN             ELEMENT NOT FOUND                            
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOSKCDE,WIOSKIDQ   DONE IF NOT STATUS ELEMENT                   
         BNE   VRSQNDN                                                          
*                                                                               
         MVC   SVSTAELM,0(R6)      SAVE FOUND ELEMENT                           
*                                                                               
VRSQNCN  DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOSKEY  GET NEXT ELEMENT                            
*                                                                               
         B     VRSQNLP                                                          
*                                                                               
VRSQNDN  DS    0H                                                               
*                                                                               
         LA    R6,ELEMENT          RE-POINT TO NEW STATUS ELEMENT               
*                                                                               
         OC    SVSTAELM,SVSTAELM   IF ELEMENT FOUND                             
         BZ    VRSQND1                                                          
*                                                                               
         CLI   QSTAT,WIOSSNTQ      IF STAUS SENT OR RESENT                      
         BE    *+8                                                              
         CLI   QSTAT,WIOSRSTQ                                                   
         BNE   VRSQND0                                                          
*                                                                               
         CLC   QSTAT,WIOSSTAT-WIOSKEY+SVSTAELM SKIP IF STATUS UNCHANGED         
         BE    VRLNK               SAME MESSAGE FROM AB AND WS                  
*                                                                               
VRSQND0  DS    0H                                                               
*                                                                               
         MVC   WIOSKSQN,WIOSKSQN-WIOSKEY+SVSTAELM COPY SQN                      
*                                                                               
VRSQND1  DS    0H                                                               
*                                                                               
         MVI   WIOSKLEN,WIOSDRLQ   SET ELEMENT LENGTH                           
*                                                                               
         SR    RF,RF               BUMP SQN                                     
         IC    RF,WIOSKSQN                                                      
         AHI   RF,1                                                             
         STC   RF,WIOSKSQN                                                      
*                                                                               
         MVC   WIOSSTAT,QSTAT      SAVE STATUS                                  
*                                                                               
         TITLE 'PPWIO30 - GET DATE AND TIME - VRDTE'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO GET CURRENT DATE AND TIME                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRDTE    DS    0H                                                               
*                                                                               
*        ADD TRAILING BLANKS TO HIST ID                                         
*                                                                               
         LA    R0,L'LNKHSTID       LENGTH OF HISTID                             
         LA    R1,LNKHSTID+L'LNKHSTID-1  END OF FIELD                           
*                                                                               
         CLI   0(R1),C' '          DONE IF NON-PRINTABLE CH                     
         BH    *+16                                                             
         MVI   0(R1),C' '          ELSE FORCE TO SPACE                          
         SHI   R1,1                BACK UP A BYTE                               
         BCT   R0,*-16                                                          
*                                                                               
         GOTOR DATCON,DMCB,(5,0),(25,WIOSDATE) GET DATE AND TIME                
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOSTIME         ADD 6 HOURS TO TIME                          
         AHI   RF,6                   ALLOWS FOR DDS CLOCK                      
         STC   RF,WIOSTIME                                                      
*                                                                               
         LA    R2,SSTDTEH          POINT TO DATE FIELD                          
*                                                                               
         GOTOR DATCON,DMCB,(3,WIOSDATE),(17,FLDDATA) DISPLAY DATE               
*                                                                               
         LA    R2,SSTTIMH          POINT TO TIME FIELD                          
*                                                                               
         GOTOR DISTIM,DMCB,WIOSTIME,FLDDATA                                     
*                                                                               
VRDTEX   DS    0H                                                               
*                                                                               
         MVC   WIOSPID,SVWIOPID    FILL IN PID                                  
*                                                                               
*        ADD STATUS ELEMENT                                                     
*                                                                               
VRFADD   DS    0H                                                               
*                                                                               
         L     RF,ADDELM           WILL BE ADDING ELEMENTS                      
*                                                                               
         GOTOR (RF),DMCB,ELEMENT   ADD ELEMENT TO MINIO SET                     
*                                                                               
         MVC   SVSTAELM,ELEMENT    SAVE NEW ELEMENT                             
*                                                                               
*        READ IN HEADER ELEMENT                                                 
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING WIOHDRD,R6          ESTABLISH HEADER ELEMENT                     
*                                                                               
         MVI   WIOHKCDE,WIOHKIDQ   SET HEADER ELEMENT CODE                      
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT FIND HEADER ELEMENT                          
         BZ    *+6                                                              
         DC    H'0'                NO ERRORS TOLERATED                          
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
*        ANALYZE THE HIERARCHY OF THE STATUSES                                  
*                                                                               
         CLI   WIOHSTAT,WIOSAPPQ   STAUS APPROVED                               
         BE    *+8                                                              
         CLI   WIOHSTAT,WIOSREJQ   OR REJECTED                                  
         BE    VRFHDRX                NOT REPLACED                              
*                                                                               
         CLI   WIOSSTAT-WIOSKEY+SVSTAELM,WIOSRSTQ IF INCOMING RESENT            
         BE    *+8                                                              
         CLI   WIOSSTAT-WIOSKEY+SVSTAELM,WIOSSNTQ OR SENT                       
         BNE   VRFHDR10                                                         
*                                                                               
         CLI   WIOHSTAT,WIOSSNTQ      IF CURRENTLY SENT                         
         BE    *+8                                                              
         CLI   WIOHSTAT,WIOSUDLQ      OR UNDELIVERED                            
         BE    *+8                                                              
         CLI   WIOHSTAT,WIOSGENQ      OR GENERATED                              
         BE    *+8                                                              
         CLI   WIOHSTAT,WIOSRSTQ      OR RESENT                                 
         BE    VRFHDR                    UPDATE STATUS                          
*                                                                               
         B     VRFHDRX             ELSE NO UPDATE                               
*                                                                               
VRFHDR10 DS    0H                                                               
*                                                                               
         CLI   WIOSSTAT-WIOSKEY+SVSTAELM,WIOSDLVQ IF INCOMING DELIVERED         
         BE    *+8                                                              
         CLI   WIOSSTAT-WIOSKEY+SVSTAELM,WIOSUDLQ OR UNDELIVERED                
         BNE   VRFHDR20                                                         
*                                                                               
         CLI   WIOHSTAT,WIOSACCQ   DON'T OVERRIDE ACCESSED                      
         BE    VRFHDRX                                                          
*                                                                               
         B     VRFHDR                                                           
*                                                                               
VRFHDR20 DS    0H                                                               
*                                                                               
VRFHDR   DS    0H                                                               
*                                                                               
         MVC   WIOHSTAT,WIOSSTAT-WIOSKEY+SVSTAELM  SET LATEST STATUS            
*                                                                               
         GOTOR WRTELM,DMCB,WIOHKEY RE-WRITE HEADER ELEMENT                      
*                                                                               
VRFHDRX  DS    0H                                                               
*                                                                               
*        UPDATE/ADD GROUP HEADER ELEMENT                                        
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING WIOFAXD,R6          ESTABLISH FAX/EMAIL HEADER KEY               
*                                                                               
         XC    SVFAXELM,SVFAXELM   INIT ELEMENT SAVEAREA                        
*                                                                               
*        FIND CURRENT FAX/EMAIL HEADER ELEMENT                                  
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET ELEMENT ID                               
         MVI   WIOFKLEN,1          SEARCH ON ANY FAX/EMAIL ELEMENT              
*                                                                               
         MVI   HALF,X'FF'          INIT WORKAREA                                
*                                                                               
         GOTOR GETELM,DMCB,WIOFKEY FIND FIRST FAX/EMAIL ELEMENT                 
         BNZ   VRFFAXDN            NOT FOUND                                    
*                                                                               
VRFFAXLP DS    0H                                                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOFKCDE,WIOFKIDQ   DONE IF NOT A FAX/EMAIL ELEMENT              
         BNE   VRFFAXDN                                                         
*                                                                               
         CLI   WIOFKTYP,WIOFKHDQ   IF NOT A HEADER ELEMENT                      
         BE    *+12                                                             
         MVI   HALF,X'FF'             SET SWITCH                                
         B     VRFFAXCN               AND SKIP                                  
*                                                                               
         MVI   HALF,0              ELSE RE-SET SWITCH                           
*                                                                               
         XC    SVFAXELM,SVFAXELM   INIT ELEMENT SAVEAREA                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOFKLEN         GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVFAXELM(0),WIOFKEY SAVE FAX/EMAIL ELEMENT                       
*                                                                               
         OC    WIOFGPID,WIOFGPID   IF THERE IS A GROUP ID                       
         BZ    *+14                                                             
         CLC   WIOFGPID,LNKHSTID       USE THIS GROUP IF HISTID MATCH           
         B     VRFFAXFD                                                         
*                                                                               
VRFFAXCN DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOFKEY FIND NEXT ELEMENT                            
*                                                                               
         B     VRFFAXLP                                                         
*                                                                               
VRFFAXFD DS    0H                                                               
*                                                                               
         LA    R6,SVFAXELM         POINT TO MOST RECENT HEADER ELM              
*                                                                               
         B     VRFFAX07                                                         
*                                                                               
VRFFAXDN DS    0H                                                               
*                                                                               
         LA    R6,SVFAXELM         POINT TO MOST RECENT HEADER ELM              
*                                                                               
         CLI   QSTAT,WIOSSNTQ      IF STATUS SENT                               
         BE    *+8                                                              
         CLI   QSTAT,WIOSRSTQ      OR RE-SENT                                   
         BNE   VRFFAX01                                                         
*                                                                               
         CLI   HALF,X'FF'             IF SWITCH ON                              
         BE    VRFFAX05                  CREATE NEW GROUP                       
*                                                                               
VRFFAX01 DS    0H                                                               
*                                                                               
         OC    SVFAXELM,SVFAXELM   SKIP IF NO FAX ELEMENTS                      
         BZ    VRFFAXX                                                          
*                                                                               
         GOTOR GETELM,DMCB,WIOFKEY RE-READ ELEMENT                              
*                                                                               
         B     VRFFAX07                                                         
*                                                                               
VRFFAX05 DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOFKGP#         GET LATEST GROUP NUMBER                      
*                                  MAYBE 0 IF NO FAX/EMAIL ELMS FOUND           
         AHI   RF,1                BUMP SQN BY ONE                              
*                                                                               
         LA    R6,ELEMENT          POINT TO NEW ELM BUILD AREA                  
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET AS FAX/EMAIL ELEMENT                     
         MVI   WIOFKLEN,WIOFHDRL   SET ELEMENT LENGTH                           
         STC   RF,WIOFKGP#         SET GROUP NUMBER                             
         MVI   WIOFKTYP,WIOFKHDQ   SET GROUP HEADER ID                          
         MVC   WIOFGPID,LNKHSTID   SET GROUP ID                                 
*                                                                               
VRFFAX07 DS    0H                                                               
*                                                                               
         CLI   QSTAT,WIOSSNTQ      IF SENT                                      
         BE    *+8                                                              
         CLI   QSTAT,WIOSRSTQ      OR RE-SENT                                   
         BNE   VRFFAX10                                                         
*                                                                               
         CLI   HALF,X'FF'          SKIP IF NOT NEW GROUP                        
         BNE   VRFFAX20                                                         
*                                                                               
         MVC   WIOFSTAT,WIOSSTAT-WIOSKEY+SVSTAELM  SET LATEST STATUS            
*                                                                               
         MVC   WIOFSDTE,WIOSDATE-WIOSKEY+SVSTAELM  SET SENT DATE                
         MVC   WIOFSTIM,WIOSTIME-WIOSKEY+SVSTAELM  SET SENT TIME                
*                                                                               
         B     VRFFAX20                                                         
*                                                                               
VRFFAX10 DS    0H                                                               
*                                                                               
         CLI   WIOFSTAT,WIOSDLVQ   IF DELIVERED                                 
         BE    *+8                                                              
         CLI   WIOFSTAT,WIOSUDLQ   OR UNDELIVERD                                
         BE    *+8                                                              
         CLI   WIOFSTAT,WIOSAPPQ   OR APPROVED                                  
         BE    *+8                                                              
         CLI   WIOFSTAT,WIOSREJQ   OR REJECTED                                  
         BNE   VRFFAX20                                                         
*                                                                               
         MVC   WIOFDDTE,WIOSDATE-WIOSKEY+SVSTAELM  SET DATE                     
         MVC   WIOFDTIM,WIOSTIME-WIOSKEY+SVSTAELM  SET TIME                     
*                                                                               
VRFFAX20 DS    0H                                                               
*                                                                               
         L     RF,WRTELM           ASSUME RE-WRITING ELEMENT                    
*                                                                               
         CLI   QSTAT,WIOSSNTQ      IF STATUS SENT                               
         BE    *+8                                                              
         CLI   QSTAT,WIOSRSTQ      OR RE-SENT                                   
         BNE   VRFFAX22                                                         
*                                                                               
         CLI   HALF,X'FF'          SKIP IF NOT NEW GROUP                        
         BNE   VRFFAX22                                                         
*                                                                               
         L     RF,ADDELM              ADD THE NEW ELEMENT                       
*                                                                               
VRFFAX22 DS    0H                                                               
*                                                                               
         GOTOR (RF),DMCB,WIOFKEY   PUT ELEMENT IN MINIO SET                     
         BZ    *+6                 NO ERRORS                                    
         DC    H'0'                                                             
*                                                                               
VRFFAXX  DS    0H                                                               
*                                                                               
*        UPDATE ACTIVITY ELEMENT                                                
*                                                                               
         BRAS  RE,UPDACT           UPDATE ACTIVITY                              
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
         CLI   MINERR,0            MUST SUCCEED                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        CALL LINKIO INTERFACE IF NEEDED                                        
*                                                                               
VRLNK    DS    0H                                                               
*                                                                               
         LA    R6,SVSTAELM                                                      
         USING WIOSKEY,R6          ESTABLISH STATUS ELEMENT                     
*                                                                               
         CLI   DDLNKSW,C'Y'        IF IN A LINK CALL                            
         BNE   VRLNKX                                                           
*                                                                               
*        SEND NORMAL REPLY TO CALLER                                            
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)                                                      
         USING LIOBD,R3            ESTABLISH LINKIO INTERFACE BLOCK             
*                                                                               
         ST    R9,LIOBASB1         SET WORK BASE ADDRESS                        
         ST    RA,LIOBASB2         SET SCREEN BASE ADDRESS                      
*                                                                               
*        MAKE SURE WE HAVE READ TO THE END OF THE FILE                          
*                                                                               
         TM    LIOBFLG2,LIOBFEOF   TEST FOR EOF ALREADY FOUND                   
         BO    VLIOPUT2                                                         
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAGET',LIOBD) GET END OF DATA                   
*                                                                               
         TM    LIOBFLG2,LIOBFEOF   MUST BE EOF                                  
         BO    *+6                                                              
         DC    H'0'                ALL DATA FIELDS SHOULD BE PROC'D             
*                                                                               
VLIOPUT2 DS    0H                                                               
*                                                                               
*        RETURN RECORD MAPCODE                                                  
*                                                                               
         LA    R0,E#IOSTRP         SET RECORD MAP CODE                          
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',(R0))                  
*                                                                               
*        RETURN IO NUMBER                                                       
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#IOLKEY),    X        
               ('LD_CHARQ',LNKWIOKY),(L'LNKWIOKY,0)                             
*                                                                               
*        RETURN IO STATUS DATE                                                  
*                                                                               
         GOTOR DATCON,DMCB,(3,WIOSDATE),(17,WORK) DISPLAY DATE                  
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#STATDT),    X        
               ('LD_CHARQ',WORK),(8,0)                                          
*                                                                               
*        RETURN IO STATUS TIME                                                  
*                                                                               
         LA    R2,FLDH             POINT TO WORK FIELD                          
         XC    FLD,FLD             INIT FIELD                                   
*                                                                               
         GOTOR DISTIM,DMCB,WIOSTIME                                             
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#STATTM),    X        
               ('LD_CHARQ',FLDDATA),(8,0)                                       
*                                                                               
VRLNKX   DS    0H                                                               
*                                                                               
         MVC   QSSQN,WIOSKSQN      SET SEQUENCE NUMBER                          
*                                                                               
         BRAS  RE,DR               DISPLAY STATUS                               
*                                                                               
VRX      DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPWIO00 - PRINT NEW INSORD CONTROLLER - VRERR '                 
***********************************************************************         
*                                                                     *         
*        VALREC ERROR MESSAGES                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
VRSTANV  DS    0H                                                               
VRSTAE1  LHI   RF,PPEDTENV         STATUS NOT VALID                             
         J     VRERR                                                            
*                                                                               
VRERR    DS    0H                                                               
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         XIT1                      DOWNLOADS AND STATUS RETURN                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPWIO00 - PRINT NEW INSORD CONTROLLER - DWNLOAD'                
***********************************************************************         
*                                                                     *         
*        IO DOWNLOAD                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
DWNLOAD  NTR1  BASE=*,LABEL=*      IO DOWNLOAD                                  
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         LHI   RF,E#IODWRP                                                      
         STCM  RF,3,LNKREPCD       SET LINK REPLY CODE                          
*                                                                               
         GOTOR GETINPUT,DMCB,LNKAFLT   STORE INPUT                              
*                                                                               
DWLOOP   DS    0H                                                               
*                                                                               
*        FIND NEXT LIST OF FILTERS                                              
*                                                                               
         BRAS  RE,GETFLTS             GET FILTERS                               
*                                                                               
         CLI   LNKFLSW,C'L'        EXIT IF END OF LIST FOUND                    
         BE    DWDONE                                                           
*                                                                               
*        FIND FIRST MINIO SET TO SATISFY FILTERS                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH WEB IO PERIOD PASSIVE              
         USING WIO1KEYD,R4                                                      
*                                                                               
         MVC   WIO1AGY,QAGY        SET AGENCY                                   
         MVC   WIO1MED,QFMED       SET MEDIA                                    
         MVI   WIO1RCD,WIO1RCDQ    SET RECORD CODE                              
*                                                                               
         OC    QFCLT,QFCLT         DONE IF NO CLIENT FILTER                     
         BZ    DWKEYX                                                           
*                                                                               
         MVC   WIO1CLT,QFCLT       SET CLIENT                                   
*                                                                               
         OC    QFPRD,QFPRD         DONE IF NO PRODUCT FILTER                    
         BZ    DWKEYX                                                           
*                                                                               
         MVC   WIO1PRD,QFPRD       SET PRD CODE                                 
*                                                                               
         OC    QFPUB,QFPUB         DONE IF NO PUB FILTER                        
         BZ    DWKEYX                                                           
*                                                                               
         MVC   WIO1PBCD,QFPUB      SET BASE PUB CODE                            
*                                                                               
         OC    QFPER,QFPER         DONE IF NO PERIOD FILTER                     
         BZ    DWKEYX                                                           
*                                                                               
         GOTOR DATCON,DMCB,(3,QFSTART),(2,WIO1END)  PERIOD                      
*                                                                               
DWKEYX   DS    0H                                                               
*                                                                               
         GOTOR HIGH                READ DIRECTORY                               
*                                                                               
DWKYLP   DS    0H                                                               
*                                                                               
         CLC   WIO1KEYD(WIO1CLT-WIO1KEY),KEYSAVE  MATCH ON REC TYPE             
         BNE   DWKYDN              AND MEDIA                                    
*                                                                               
         CLI   LNKREQTP,LNKRECRQ   IF ONLY MOST RECENT REVISION                 
         BNE   *+14                                                             
         CLC   WIO1KEYD(WIO1RV#-WIO1KEY),KEYSAVE   CONTINUE ON IO CHG           
         BE    DWKYCN                                                           
*                                                                               
         OC    QFCLT,QFCLT         IF CLIENT FILTER GIVEN                       
         BZ    *+14                                                             
         CLC   WIO1CLT,QFCLT          FILTER ON CLIENT                          
         BNE   DWKYDN                                                           
*                                                                               
         OC    QFPRD,QFPRD         IF PRODUCT FILTER GIVEN                      
         BZ    *+14                                                             
         CLC   WIO1PRD,QFPRD          FILTER ON PRODUCT                         
         BNE   DWKYDN                                                           
*                                                                               
         OC    QFPUB,QFPUB         IF PUB   FILTER GIVEN                        
         BZ    DWKYLP10                                                         
*                                                                               
         CLC   WIO1PUB,QFPUB          FILTER ON PUB                             
         BE    DWKYLP10                                                         
*                                                                               
         CLC   WIO1PBCD,QFPUB         DONE IF NOT SAME BASE PUB                 
         BNE   DWKYDN                                                           
*                                                                               
         OC    WIO1ZONE(2),WIO1ZONE   SKIP IF NOT BASE PUB                      
         BNZ   DWKYCN                                                           
*                                                                               
DWKYLP10 DS    0H                                                               
*                                                                               
         OC    QFPER,QFPER         IF PERIOD FILTER GIVEN                       
         BZ    DWKYPERN                                                         
*                                                                               
         GOTOR DATCON,DMCB,(2,WIO1END),(3,WORK)  TRANSLATE DATE                 
*                                                                               
         CLC   QFSTART,WORK        SKIP IF ENDS BEFORE PERIOD START             
         BH    DWKYCN                                                           
*                                                                               
         GOTOR DATCON,DMCB,(2,WIO1STRT),(3,WORK)  TRANSLATE DATE                
*                                                                               
         CLC   QFEND,WORK          SKIP IF STARTS AFTER PERIOD END              
         BL    DWKYCN                                                           
*                                                                               
DWKYPERN DS    0H                                                               
*                                                                               
         MVC   SVWIO1KY,WIO1KEY    SAVE KEY INTO SVWIO1KEY                      
*                                                                               
         GOTOR GETREC              READ IN MASTER REORD                         
*                                                                               
         L     RF,AIO              POINT TO FOUND RECORD                        
*                                                                               
         MVC   KEY,0(RF)           COPY MASTER KEY                              
         MVC   QIOKEY,0(RF)                                                     
*                                                                               
         LA    R4,KEY                                                           
         USING WIOKEY,R4           ESTABLISH IO KEY                             
*                                                                               
         GOTOR HIGH                SET FILE POINTERS                            
*                                                                               
*        DETERMINE REVISIONS TO PROCESS                                         
*                                                                               
         XC    SVIOKEY,SVIOKEY     INIT KEY SAVEAREA                            
*                                                                               
DWRVLOOP DS    0H                                                               
*                                                                               
         CLC   WIOKEY(WIOKRV#-WIOKEY),QIOKEY MUST BE SAME IO#                   
         BNE   DWRVDONE                                                         
*                                                                               
         CLC   WIOKELMK,=7X'FF'    SKIP IF NOT MASTER KEY                       
         BNE   DWRVCONT                                                         
*                                                                               
         CLI   LNKREQTP,LNKRECRQ   IF ONLY MOST RECENT REVISION                 
         BNE   DWRVRECN                                                         
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         MVC   MINMKEY,KEY         SET MASTER KEY                               
*                                                                               
         MVI   MINDELSW,C'Y'       READ DELETED MINIO SETS                      
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0            NO ERRORS TOLERATED                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        FIND HEADER ELEMENT AND DISPLAY PERIOD                                 
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR ELEMENT WORKAREA                       
*                                                                               
         LA    R6,ELEMENT          BUILD HEADER ELEMENT KEY                     
         USING WIOHKEY,R6                                                       
*                                                                               
         MVI   WIOHKCDE,WIOHKIDQ   SET HEADER ELM CODE                          
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT READ FOR HEADER ELEMENT                      
         BNE   DWRVCN10            DROP - PROBABLY DELETED                      
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOHSTAT,WIOSGENQ   SKIP GENERATED                               
         BE    DWRVCN10                                                         
*                                  ELSE                                         
         MVC   SVIOKEY,MINMKEY        SAVE CURRENT KEY                          
*                                                                               
         B     DWRVCN10                                                         
*                                                                               
DWRVRECN DS    0H                                                               
*                                                                               
         BRAS  RE,ST               RETURN STATUS DATA                           
*                                                                               
         CLI   LNKREQTP,LNKONERQ   DONE IF SINGLE IO WANTED                     
         BE    DWRVDONE                                                         
*                                                                               
DWRVCN10 DS    0H                                                               
*                                                                               
*        RESTORE FILE POINTERS                                                  
*                                                                               
         GOTOR HIGH                                                             
*                                                                               
DWRVCONT DS    0H                                                               
*                                                                               
         GOTOR SEQ                 READ NEXT IO KEY                             
*                                                                               
         B     DWRVLOOP                                                         
*                                                                               
DWRVDONE DS    0H                                                               
*                                                                               
         CLI   LNKREQTP,LNKRECRQ   IF ONLY MOST RECENT REVISION                 
         BNE   DWRVDN1                                                          
*                                                                               
         OC    SVIOKEY,SVIOKEY     SKIP IF NO KEY FOUND                         
         BZ    DWRVDN1                                                          
*                                                                               
         LA    R4,KEY                                                           
*                                                                               
         MVC   KEY,SVIOKEY         RESTORE LAST FOUND KEY                       
*                                                                               
         MVC   QIOKEY,WIOKEY       UPDATE IOKEY                                 
*                                                                               
         BRAS  RE,ST               SEND STATUS                                  
*                                                                               
DWRVDN1  DS    0H                                                               
*                                                                               
         MVC   KEY,SVWIO1KY        RESTORE PERIOD POINTER                       
*                                                                               
         GOTOR HIGH                RESET FIELD POINTERS                         
*                                                                               
DWKYCN   DS    0H                                                               
*                                                                               
         GOTOR SEQ                 READ NEXT POINTER ON FILE                    
*                                                                               
         B     DWKYLP                                                           
*                                                                               
DWKYDN   DS      0H                                                             
*                                                                               
DWCONT   DS    0H                                                               
         B     DWLOOP                                                           
*                                                                               
DWDONE   DS    0H                                                               
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)                                                      
         USING LIOBD,R3            ESTABLISH LINKIO INTERFACE BLOCK             
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOACLO',LIOBD) CLOSE WORKER FILE                 
*                                                                               
DWNLOADX DS    0H                                                               
         XIT1                         ALL DONE                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPWIO00 - PRINT NEW INSORD CONTROLLER - VRPSV'                  
***********************************************************************         
*                                                                     *         
*        UPDATE PASSIVE POINTERS                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
NEWPSV   NTR1  BASE=*,LABEL=*      DISPLAY RECORD                               
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING MINBLKD,R7                                                       
*                                                                               
         IC    R0,DMINBTS          SAVE SETTING                                 
*                                                                               
         STC   R0,DMINBTS          RESTORE SETTING                              
*                                                                               
NEWPSVX  DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPWIO00 - PRINT NEW INSORD CONTROLLER - UPDACT'                 
***********************************************************************         
*                                                                     *         
*        UPDATE ACTIVITY ELEMENT                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
UPDACT   NTR1  BASE=*,LABEL=*      UPDATE ACTIVITY ELEMENT                      
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING MINBLKD,R7                                                       
*                                                                               
**       MVC   WIOCPID,SVWIOPID                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(25,WORK)  GET TODAY/TIME                      
*                                                                               
         MVC   SVDATE,WORK         SAVE DATE                                    
         MVC   SVTIME,WORK+3       SAVE TIME AS BINARY                          
*                                                                               
*        FIND PRIOR ACTIVITY ELEMENT FOR E-MAIL                                 
*@@@@@                                                                          
*@@@@@   XC    ELEMENT,ELEMENT                                                  
*@@@@@   USING WIOACTHD,ELEMENT    ESTABLISH ACTIVITY ELEMENT                   
*@@@@@                                                                          
*@@@@@   MVI   WIOAKCDE,WIOAKDTQ   SET AS DETAIL ACTIVITY ELEMENT               
*@@@@@   MVI   WIOAKLEN,WIOAKCSQ-WIOAKEY   SET TO FIND ANY ACT ELM              
*@@@@@   MVC   WIOAKDSQ,WIOSKSQN   SET DETAIL SEQ NUMBER                        
*@@@@@   MVI   WIOAKACT,WIOAKACQ   SET AS ACTIVITY ELEMENT                      
*@@@@@                                                                          
*@@@@@   GOTO1 GETELM,DMCB,ELEMENT GET FIRST ACTIVITY ELEMENT                   
*@@@@@                                                                          
UACTLOOP DS    0H                                                               
*@@@@@                                                                          
*@@@@@   BNZ   UACTDONE            END OF ACTIVITY ELEMENTS                     
*@@@@@                                                                          
*@@@@@   L     R1,MINELEM          POINT TO FOUND ELEMENT                       
*@@@@@   SR    RF,RF                                                            
*@@@@@   IC    RF,1(R1)            GET ELEMENT LENGTH                           
*@@@@@   BCTR  RF,0                DECREMENT FOR EXECUTE                        
*@@@@@   EX    RF,*+8                                                           
*@@@@@   B     *+10                                                             
*@@@@@   MVC   WIOACTHD(0),0(R1)   MOVE ELEMENT TO WORK AREA                    
*@@@@@                                                                          
UACTCONT DS    0H                                                               
*@@@@@                                                                          
*@@@@@   MVI   WIOAKLEN,WIOAKCSQ-WIOAKEY   FILTER ON ACTIVITY ELM               
*@@@@@                                                                          
*@@@@@   GOTOR NXTELM,DMCB,ELEMENT  FIND NEXT ELEMENT                           
*@@@@@                                                                          
*@@@@@   B     UACTLOOP                                                         
*@@@@@                                                                          
UACTDONE DS    0H                                                               
*@@@@@                                                                          
*@@@@@   MVI   ACTSW,C'N'          ASSUME NEW ACTIVITY ELEMENT                  
*@@@@@                                                                          
*@@@@@   CLC   SVWIOPID,WIOAHPID   IF SAME PERSON                               
*@@@@@   BNE   *+10                                                             
*@@@@@   CLC   SVDATE,WIOAHDTE     AND SAME DATE                                
*@@@@@   BNE   *+12                                                             
*@@@@@   MVI   ACTSW,C'O'              FLAG AS OLD ELEMENT                      
*@@@@@   B     UACT10                                                           
*@@@@@                             ELSE                                         
*@@@@@   SR    RF,RF                                                            
*@@@@@   ICM   RF,3,WIOAKCSQ          BUMP THE SEQUENCE NUMBER                  
*@@@@@   AHI   RF,1                                                             
*@@@@@   STCM  RF,3,WIOAKCSQ                                                    
*@@@@@                                                                          
*@@@@@   XC    WIOACHGS,WIOACHGS   CLEAR CHANGES INDICATORS                     
*@@@@@                                                                          
UACT10   DS    0H                                                               
*@@@@@                                                                          
*@@@@@   MVI   WIOAKLEN,WIOACTLQ   SET ELEMENT LENGTH                           
*@@@@@                                                                          
*@@@@@   MVC   WIOAHPID,SVWIOPID   SET PID                                      
*@@@@@   MVC   WIOAHDTE,SVDATE     SET DATE                                     
*@@@@@                                                                          
*@@@@@   SET ACTION INDICATOR                                                   
*@@@@@                                                                          
*@@@@@   CLI   ACTNUM,ACTADD       CHECK FOR ADD                                
*@@@@@   BNE   *+12                                                             
*@@@@@   OI    WIOAHCH1,WIOADADD     SET INDICATOR                              
*@@@@@   B     UACTACTX                                                         
*@@@@@                                                                          
*@@@@@   CLI   ACTNUM,ACTDEL       CHECK FOR DEL                                
*@@@@@   BNE   *+12                                                             
*@@@@@   OI    WIOAHCH1,WIOADDEL     SET INDICATOR                              
*@@@@@   B     UACTACTX                                                         
*@@@@@                                                                          
*@@@@@   CLI   ACTNUM,ACTREST      CHECK FOR RESTORE                            
*@@@@@   BNE   *+12                                                             
*@@@@@   OI    WIOAHCH1,WIOADRES     SET INDICATOR                              
*@@@@@   B     UACTACTX                                                         
*@@@@@                                                                          
UACTACTX DS    0H                                                               
*@@@@@                                                                          
*@@@@@   COMPARE FIELD BY FIELD FOR CHANGES                                     
*@@@@@                                                                          
*@@@@@   USING WIOHDRD,SVSSTOLD   ESTABLISH SECOND DETAIL ELM                   
*@@@@@                                                                          
*@@@@@   CLC   WIODPUB,A.WIODPUB   PUB CHANGED?                                 
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH1,WIOADPUB      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODDTE,A.WIODDTE   RUN DATE CHANGED?                            
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH1,WIOADDTE      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODSPC,A.WIODSPC   SPACE CHANGED?                               
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH1,WIOADSPC      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODACAP,A.WIODACAP CAPTION CHANGED?                             
*@@@@@   BNE   *+10                                                             
*@@@@@   CLC   WIODACP2,A.WIODACP2 CAPTION 2 CHANGED?                           
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH1,WIOADCAP      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODNCL,A.WIODNCL   # OF COLORS CHANGED?                         
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH1,WIOADNCL      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODCDST,A.WIODCDST CASH DISCOUNT STATUS                         
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH2,WIOADCD       YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODGST,A.WIODGST   GST CHANGED?                                 
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH2,WIOADGST      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODPST,A.WIODPST   PST CHANGED?                                 
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH2,WIOADPST      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODSREP,A.WIODSREP REP CHANGED?                                 
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH2,WIOADREP      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODRATE,A.WIODRATE RATE CHANGED?                                
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH2,WIOADRTE      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODGRS,A.WIODGRS   GROSS CHANGED?                               
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH2,WIOADGRS      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODNET,A.WIODNET   NET CHANGED?                                 
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH2,WIOADNET      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODPREM,A.WIODPREM PREMIUM CHANGED?                             
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH2,WIOADPRM      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODCLT,A.WIODCLT   CLIENT CHANGED?                              
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH3,WIOADCLT      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODSER#,A.WIODSER# BUY SER# CHANGED?                            
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH3,WIOADBS#      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODPRD,A.WIODPRD   PRD CHANGED?                                 
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH3,WIOADPRD      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODEST,A.WIODEST   ESTIMATE CHANGED?                            
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH3,WIOADEST      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODBYDT,A.WIODBYDT BUY LINE DATE CHANGED?                       
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH3,WIOADBDT      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODLIN#,A.WIODLIN# BUY LINE CHANGED?                            
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH3,WIOADBLN      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODAIMP,A.WIODAIMP ACTUAL IMPRESSIONS CHANGED?                  
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH3,WIOADIMP      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIODACPM,A.WIODACPM ACTUAL CPMS CHANGED?                         
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH3,WIOADCPM      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   CLC   WIOD#LIN,A.WIOD#LIN NUMBER OF INSERTIONS                         
*@@@@@   BE    *+8                                                              
*@@@@@   OI    WIOAHCH4,WIOADLNS      YES - SET INDICATOR                       
*@@@@@                                                                          
*@@@@@   PUT ACTIVITY ELEMENT IN MINIO SET                                      
*@@@@@                                                                          
*@@@@@   L     RF,ADDELM           ASSUMING THIS A NEW ELEMENT                  
*@@@@@                                                                          
*@@@@@   CLI   ACTSW,C'O'          IF THIS AN OLD ELEMENT                       
*@@@@@   BNE   *+8                                                              
*@@@@@   L     RF,WRTELM              WRITE OLD ELEMENT                         
*@@@@@                                                                          
*@@@@@   GOTOR (RF),DMCB,ELEMENT   PUT ELEMENT IN MINIO SET                     
*@@@@@   CLI   MINERR,0            MUST SUCCEED                                 
*@@@@@   BE    *+6                                                              
*@@@@@   DC    H'0'                                                             
*                                                                               
UPDACTX  DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E30 - STAT MAINT/LIST - DR'                                  
***********************************************************************         
*                                                                     *         
*        DISPLAY  STATUS FIELDS                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DR       NTR1  BASE=*,LABEL=*      DISPLAY RECORD                               
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         XC    MINMKEY,MINMKEY     INIT MASTER MINIO KEY                        
         MVC   MINMKEY(WIOKELMK-WIOKEY),QIOKEY SET MASTER KEY                   
*                                                                               
         MVI   MINDELSW,C'Y'       READ DELETED MINIO SETS                      
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
*        READ IN STATUS ELEMENT                                                 
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT          ESTABLISH DETAIL ELEMENT KEY                 
         USING WIOSTATD,R6         ESTABLISH STATUS ELEMENT                     
*                                                                               
         MVI   WIOSKCDE,WIOSKIDQ   SET ELEMENT ID                               
         MVC   WIOSKSQN,QSSQN      SET STATUS SEQ NUMBER                        
*                                                                               
         GOTOR GETELM,DMCB,WIOSKEY FIND ELEMENT                                 
         BNZ   DRSQNER             ELEMENT NOT FOUND                            
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
*                                                                               
         L     R1,MINELEM          POINT TO FOUND ELEMENT                       
         SR    RF,RF                                                            
         IC    RF,1(R1)            GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WIOSTATD(0),0(R1) MOVE ELEMENT TO WORK AREA                      
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST - DRGSTA'                            
***********************************************************************         
*                                                                     *         
*        DISPLAY  DISPLAY SEQ STATUS, DATE AND TIME                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRGSTA   DS    0H                                                               
*                                                                               
         LA    R2,SSTSTAH          POINT TO STATUS FIELD                        
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD HEADER                
*                                                                               
         BRAS  RE,CLRFLD           CLEAR THE FIELD                              
*                                                                               
         MVC   QSTAT,WIOSSTAT      SET STATUS                                   
*                                                                               
         GOTOR DISSTA              DISPLAY STATUS                               
*                                                                               
         BRAS  RE,SETLEN           FILL IN TRUE LENGTH OF DATA                  
*                                                                               
*        DISPLAY STATUS DATE                                                    
*                                                                               
DRDTE    DS    0H                                                               
*                                                                               
         LA    R2,SSTDTEH          POINT TO STATUS DATE                         
*                                                                               
         BRAS  RE,CLRFLD           CLEAR THE FIELD                              
*                                                                               
         OC    WIOSDATE,WIOSDATE   SKIP IF NO DATE GIVEN                        
         BZ    DRDTEX                                                           
*                                                                               
         GOTOR DATCON,DMCB,(3,WIOSDATE),(17,FLDDATA) DISPLAY DATE               
*                                                                               
DRDTEX   DS    0H                                                               
*                                                                               
*        DISPLAY STATUS TIME                                                    
*                                                                               
DRTIM    DS    0H                                                               
*                                                                               
         LA    R2,SSTTIMH          POINT TO STATUS TIME                         
*                                                                               
         BRAS  RE,CLRFLD           CLEAR THE FIELD                              
*                                                                               
         OC    WIOSTIME,WIOSTIME   SKIP IF NO TIME ENTERED                      
         BZ    DRTIMX                                                           
*                                                                               
         GOTOR DISTIM,DMCB,(3,WIOSTIME)    DISPLAY TIME                         
*                                                                               
DRTIMX   DS    0H                                                               
*                                                                               
*        DISPLAY STATUS NAME                                                    
*                                                                               
DRNAME   DS    0H                                                               
*                                                                               
         LA    R2,SSTNAMH          POINT TO STATUS NAME                         
*                                                                               
         BRAS  RE,CLRFLD           CLEAR THE FIELD                              
*                                                                               
         OC    WIOSPID,WIOSPID     SKIP IF NO PID                               
         BZ    DRNAMEX                                                          
*                                                                               
         GOTOR TRNPID,DMCB,WIOSPID    DISPLAY USER'S NAME                       
*                                                                               
DRNAMEX  DS    0H                                                               
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST - DRSTAT'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO DISPLAY  STAT HISTORY                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRSTAT DS      0H                                                               
*                                                                               
*        CHECK IF TRANSFERRING TO PFM                                           
*                                                                               
         CLI   PFAID,9             IF PFKEY9                                    
         BE    *+8                                                              
         CLI   PFAID,21            OR 21                                        
         BNE   DRPFMX                                                           
*                                                                               
         GOTOR GOPFM                                                            
*                                                                               
DRPFMX   DS    0H                                                               
*                                                                               
DRX      DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
DRSQNER  LHI RF,PPESTANF           INSORD LINE ITEM NOT ON FILE                 
         J     DRERR1                                                           
*                                                                               
DRERR    DS    0H                  INSORD RECORD DOES NOT EXIST                 
*                                    CLEAR SEQ # AND PERIOD                     
*                                                                               
         LR    R0,R2               SAVE FIELD POINTER                           
*                                                                               
         LA    R2,SSTSQNH          POINT TO SEQ NUMBER FIELD                    
         BRAS  RE,CLRFLD           CLEAR INSORD SERIAL NUMBER FIELD             
*                                                                               
         LA    R2,SSTPERH          POINT TO PERIOD FIELD                        
         BRAS  RE,CLRFLD           CLEAR INSORD PERIOD FIELD                    
*                                                                               
         LR    R2,R0               RESTORE FIELD POINTER                        
*                                                                               
DRERR1   DS    0H                                                               
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         XIT1                      DOWNLOADS AND STATUS RETURN                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST - DK'                                
***********************************************************************         
*                                                                     *         
*        DISPLAY  KEY FROM SELECTION ON LIST SCREEN                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DK       NTR1  BASE=*,LABEL=*      DISPLAY RECORD                               
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         XC    MINMKEY,MINMKEY     INIT MASTER MINIO KEY                        
         MVC   MINMKEY(WIOKELMK-WIOKEY),KEY    SET MASTER KEY                   
*                                                                               
         USING WIORECD,MINMKEY     ESTABLISH MASTER KEY                         
*                                                                               
         MVI   MINDELSW,C'Y'       READ DELETED MINIO SETS                      
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
         SR    RF,RF                                                            
         ICM   RF,1,SELLISTN       GET RELATIVE NUMBER IN LIST                  
         MHI   RF,L'SVLSTSST       CALCULATE INDEX                              
         LA    RF,SVLSTSST(RF)     INDEX TO ENTRY  IN TABLE                     
*                                                                               
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING WIOSKEY,R6          ESTABLISH STATUS ELEMENT                     
*                                                                               
         MVC   WIOSKEY,0(RF)       SET DETAIL KEY FOR SELECTION                 
*                                                                               
DKDKYX   DS    0H                                                               
*                                                                               
         GOTO1 GETELM,DMCB,WIOSKEY FIND ELEMENT                                 
         BZ    *+6                 ELEMENT FOUND                                
         DC    H'0'                MUST FIND IT                                 
*                                                                               
         DROP  R6                                                               
*                                                                               
         L     R1,MINELEM          POINT TO FOUND ELEMENT                       
         MVC   SVHDRELM,0(R1)      MOVE ELEMENT TO WORK AREA                    
*                                                                               
*        SET MEDIA ON SCREEN                                                    
*                                                                               
         LA    R2,SSTMEDH          POINT TO MEDIA FIELD                         
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         MVC   QMED,WIOKMED        SET MEDIA                                    
*                                                                               
         GOTOR DISMED              DISPLAY MEDIA                                
*                                                                               
*        SET CLIENT ON SCREEN                                                   
*                                                                               
         LA    R2,SSTCLTH          POINT TO CLIENT FIELD                        
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         MVC   QCLT,WIOKCLT        SET CLIENT                                   
*                                                                               
         GOTOR DISCLT              DISPLAY CLIENT                               
*                                                                               
*        SET PUB ON SCREEN                                                      
*                                                                               
         LA    R2,SSTPUBH          POINT TO PUB FIELD                           
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         MVC   QPUB,WIOKPUB        SET MASTER PUB                               
*                                                                               
         GOTOR DISPUB              DISPLAY PUB CODE                             
*                                                                               
*        SET INSORD NUMBER ON SCREEN                                            
*                                                                               
         LA    R2,SSTIO#H          POINT TO INSORD # FIELD                      
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         MVC   QIO#,WIOKIO#        SET INSORD #                                 
         MVC   QREV#,WIOKRV#       SET REVISION NUMBER                          
*                                                                               
         GOTOR DISIO#              DISPLAY IO#                                  
*                                                                               
         LA    R2,SSTREV#H         POINT TO REVISION # FIELD                    
*                                                                               
         GOTOR DISRV#              DISPLAY REVISION #                           
*                                                                               
*        SET STATUS SEQ NUMBER ON SCREEN                                        
*                                                                               
         LA    R2,SSTSQNH          POINT TO STATUS SEQ NUMBER FIELD             
*                                                                               
         USING WIOSKEY,R6                                                       
         EDIT  WIOSKSQN,SSTSQN,0,ALIGN=LEFT                                     
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         STC   R0,FLDILEN          LENGTH OF INPUT                              
         OI    FLDIIND,FINPNUM     INPUT IS NUMERIC                             
*                                                                               
         CLI   ACTNUM,ACTABDEL     SKIP IF ACTION ABDELETE                      
         BE    *+8                                                              
         BRAS  RE,VK               VALIDATE KEY                                 
*                                                                               
DKX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST - DL'                                
***********************************************************************         
*                                                                     *         
*        DELETE A STATUS SEQ                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DL       NTR1  BASE=*,LABEL=*      DISPLAY RECORD                               
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO CONTROL BLOCK                
         USING MINBLKD,R7                                                       
*                                                                               
         XC    MINMKEY,MINMKEY     INIT MASTER MINIO KEY                        
         MVC   MINMKEY(WIOKELMK-WIOKEY),QIOKEY SET MASTER KEY                   
*                                                                               
         MVI   MINDELSW,C'Y'       READ DELETED MINIO SETS                      
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
*        READ IN ALL RELATED STATUS ELEMENTS AND DELETE                         
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT          ESTABLISH DETAIL ELEMENT KEY                 
         USING WIOSKEY,R6                                                       
*                                                                               
         MVI   WIOSKCDE,WIOSKIDQ   SET ELEMENT ID                               
         MVC   WIOSKSQN,QSSQN      SET SEQ     NUMBER                           
         MVI   WIOSKLEN,WIOSKSQN-WIOSKEY   FILTER ON SEQ #                      
*                                                                               
         GOTO1 GETELM,DMCB,ELEMENT FIND FIRST ELEMENT                           
*                                                                               
         DROP  R6                                                               
*                                                                               
         USING WIOSKEY,SVHDRELM    ESTABLISH STATUS ELEMENT                     
*                                                                               
DLSTATLP DS    0H                                                               
*                                                                               
         BNZ   DLSTATDN            END OF STATUS ELEMENTS                       
*                                                                               
         L     R1,MINELEM          POINT TO FOUND ELEMENT                       
         SR    RF,RF                                                            
         IC    RF,1(R1)            GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WIOSTATD(0),0(R1) MOVE ELEMENT TO WORK AREA                      
*                                                                               
         GOTOR DELELM,DMCB,SVHDRELM DELETE ELEMENT                              
*                                                                               
         GOTOR GETELM,DMCB,SVHDRELM RE-SET   FILE POINTERS                      
*                                                                               
DLSTATCN DS    0H                                                               
*                                                                               
         MVI   WIOSKLEN,WIOSKSQN-WIOSKEY   FILTER ON DETAIL SQN                 
*                                                                               
         GOTOR NXTELM,DMCB,SVHDRELM FIND NEXT ELEMENT                           
*                                                                               
         B     DLSTATLP                                                         
*                                                                               
DLSTATDN DS    0H                                                               
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
         CLI   MINERR,0            MUST SUCCEED                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DLX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST - ST'                                
***********************************************************************         
*                                                                     *         
*        RETURN STATUS DATA TO ADBUYER                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ST       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO CONTROL BLOCK                
         USING MINBLKD,R7                                                       
*                                                                               
         XC    MINMKEY,MINMKEY     INIT MASTER MINIO KEY                        
         MVC   MINMKEY(WIOKELMK-WIOKEY),QIOKEY SET MASTER KEY                   
*                                                                               
         MVI   MINDELSW,C'Y'       READ DELETED MINIO SETS                      
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
         CLI   MINERR,0                                                         
         BNE   STX                 DONE IF MINIO SET NOT FOUND                  
*                                                                               
*        CALL LINKIO INTERFACE IF NEEDED                                        
*                                                                               
         CLI   DDLNKSW,C'Y'        SKIP IF NOT IN A LINK CALL                   
         BNE   STX                                                              
*                                                                               
*        SEND NORMAL REPLY TO CALLER                                            
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)                                                      
         USING LIOBD,R3            ESTABLISH LINKIO INTERFACE BLOCK             
*                                                                               
         ST    R9,LIOBASB1         SET WORK BASE ADDRESS                        
         ST    RA,LIOBASB2         SET SCREEN BASE ADDRESS                      
*                                                                               
         LA    R4,MINMKEY          ESTABLISH IO KEY                             
         USING WIOKEY,R4                                                        
*                                                                               
*        EXPAND IO NUMBER                                                       
*                                                                               
         MVC   QMED,WIOKMED        SET MEDIA                                    
         MVC   QCLT,WIOKCLT        SET CLIENT                                   
         MVC   QPUB,WIOKPUB        SET PUB                                      
*                                                                               
         LA    R2,SSTIO#H          POINT TO IO# FIELD                           
         MVC   QIO#,WIOKIO#        SET IO NUMBER                                
         MVC   QREV#,WIOKRV#       SET REVISION NUMBER                          
*                                                                               
         GOTOR DISIO#              DISPLAY IO#                                  
*                                                                               
*        READ THROUGH MINIO SET TO GET DATA TO RETURN                           
*                                                                               
*        IO KEY DATA                                                            
*                                                                               
STIOKY   DS    0H                                                               
*                                                                               
*        READ HEADER ELEMENT                                                    
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT          ESTABLISH HEADER ELEMENT KEY                 
         USING WIOHKEY,R6                                                       
*                                                                               
         MVI   WIOHKCDE,WIOHKIDQ   SET ELEMENT ID                               
*                                                                               
         GOTO1 GETELM,DMCB,ELEMENT FIND HEADER ELEMENT                          
         BNZ   STX                 SKIP IF HEADER NOT FOUND                     
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
*        CHECK FILTERS                                                          
*                                                                               
         CLI   LNKFLTTP,LNKFALLQ   SKIP IF ALL IO'S WANTED                      
         BE    STIOPDX                                                          
*                                                                               
         CLI   LNKFLTTP,LNKFPNDQ   IF PENDING ONLY WANTED                       
         BNE   STIOPDN                                                          
*                                                                               
         CLI   WIOHSTAT,WIOSGENQ      SEND ONLY GENERATED                       
         BNE   STX                                                              
*                                                                               
         B     STIOPDX                                                          
*                                                                               
STIOPDN  DS    0H                                                               
*                                                                               
*        DROP PENDING IO'S                                                      
*                                                                               
         CLI   WIOHSTAT,WIOSGENQ      DROP GENERATED                            
         BE    STX                                                              
*                                                                               
STIOPDX  DS    0H                                                               
*                                                                               
*        SEND RECORD CODE                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,LNKREPCD       GET REPLY CODE                               
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',(R0))                  
*                                                                               
*        SEND IO KEY                                                            
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#IOLKEY),    X        
               ('LD_CHARQ',QIO#EXP),(L'QIO#EXP,0)                               
*                                                                               
         CLI   LNKSTATP,LNKCURSQ    IF CURRENT STATUS WANTED                    
         BNE   *+10                                                             
         MVC   QSTAT,WIOHSTAT          SAVE HEADER STATUS                       
*                                                                               
*        BUY DATA                                                               
*                                                                               
STBUY    DS    0H                                                               
*                                                                               
         TM    WRKDATTP,WRKBUYDQ   IF BUY DATA WANTED                           
         BNO   STBUYX                                                           
*                                                                               
*        MEDIA CODE                                                             
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#MEDCOD),    X        
               ('LD_CHARQ',QMED),(L'QMED,0)                                     
*                                                                               
*        CLIENT CODE                                                            
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CLTCOD),    X        
               ('LD_CHARQ',QCLT),(L'QCLT,0)                                     
*                                                                               
*        PRODUCT CODE                                                           
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PRDCOD),    X        
               ('LD_CHARQ',WIOHPRD),(L'WIOHPRD,0)                               
*                                                                               
*        ESTIMATE CODE IF BY SINGLE ESTIMATE                                    
*                                                                               
         TM    WIOHOTYP,WHOTESTQ   SKIP IF NOT IO FOR SINGLE ESTIMATE           
         BNO   STBUYESX                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WIOHEST        CONVERT TO 3 DIGIT CH                        
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  WORK(3),DUB+6(2)    EBCIDIC                                      
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ESTNUM),    X        
               ('LD_CHARQ',WORK),(3,0)                                          
*                                                                               
STBUYESX DS    0H                                                               
*                                                                               
*        PUB    CODE                                                            
*                                                                               
         LA    R2,SSTPUBH          POINT TO PUB FIELD                           
         GOTOR DISPUB              DISPLAY THE PUB                              
*                                                                               
         SR    R0,R0                                                            
         IC    R0,SSTPUBH+5        OUTPUT LENGTH                                
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PUBCOD),    X        
               ('LD_CHARQ',SSTPUB),((R0),0)                                     
*                                                                               
*        PERIOD                                                                 
*                                                                               
         LA    R2,SSTPERH          POINT TO PERIOD FIELD                        
         GOTOR DISPER,DMCB,WIOHSTRT   DISPLAY THE PERIOD                        
*                                                                               
         BRAS  RE,SETLEN           SET ACTUAL INPUT LENGTH                      
*                                                                               
         SR    R0,R0                                                            
         IC    R0,SSTPERH+5        OUTPUT LENGTH                                
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#STEND),     X        
               ('LD_CHARQ',SSTPER),((R0),0)                                     
*                                                                               
STBUYX   DS    0H                                                               
*                                                                               
*        HEADER DATA                                                            
*                                                                               
STHDR    DS    0H                                                               
*                                                                               
         TM    WRKDATTP,WRKHDRDQ   IF HEADER DATA WANTED                        
         BNO   STHDRX                                                           
*                                                                               
*        RETURN RUN DATE                                                        
*                                                                               
         GOTOR DATCON,DMCB,(3,WIOHDATE),(17,WORK)  CONVERT TO MMMDD/YY          
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#IORDAT),    X        
               ('LD_CHARQ',WORK),(8,0)                                          
*                                                                               
*        NUMBER OF INSERTIONS                                                   
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WIOHBUY#       GET NUMBER OF INSETIONS ON IO                
         BZ    STBUY#X                DATA NOT AVAILABLE                        
*                                                                               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  WORK(4),DUB                                                      
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#TOTINS),    X        
               ('LD_CHARQ',WORK),(4,0)                                          
*                                                                               
STBUY#X  DS    0H                                                               
*                                                                               
*        RESPONSE TIME                                                          
*                                                                               
         OC    WIOHRPDT,WIOHRPDT   SKIP IF NO DATE                              
         BZ    STHDRSPX                                                         
*                                                                               
         MVC   WORK,SPACES         INIT WORK AREA                               
*                                                                               
         GOTOR DATCON,DMCB,(3,WIOHRPDT),(17,WORK)  DATE                         
*                                                                               
         LA    R2,FLDH             POINT TO DUMMY FIELD                         
         GOTOR DISTIM,DMCB,WIOHRPTM                                             
         MVC   WORK+9(8),FLD       MOVE TO WORKAREA                             
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#RSPPER),    X        
               ('LD_CHARQ',WORK),(17,0)                                         
*                                                                               
STHDRSPX DS    0H                                                               
*                                                                               
*        SEND MANUAL STATUS INDICATOR                                           
*                                                                               
         CLI   WIOHMSND,C'M'       SKIP IF NOT MANUAL                           
         BNE   STMSNDX                                                          
*                                                                               
         MVC   WORK(6),=C'MANUAL'  SET AS MANUAL SEND                           
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#MANSEN),    X        
               ('LD_CHARQ',WORK),(6,0)                                          
*                                                                               
STMSNDX  DS    0H                                                               
*                                                                               
*        FIND SENT STATUS ELEMENT                                               
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT          ESTABLISH DETAIL ELEMENT KEY                 
         USING WIOSKEY,R6                                                       
*                                                                               
         MVI   WIOSKCDE,WIOSKIDQ   SET STATUS ELEMENT ID                        
         MVI   WIOSKLEN,1          ANY STATUS ELEMENT                           
*                                                                               
         GOTO1 GETELM,DMCB,ELEMENT FIND FIRST STATUS ELEMENT                    
         BNZ   STHDRX              HEADER NOT FOUND                             
*                                                                               
STSNTLP  DS    0H                                                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOSKCDE,WIOSKIDQ   DONE IF NOT A STATUS ELEMENT                 
         BNE   STSNTDN                                                          
*                                                                               
         CLI   WIOSSTAT,WIOSSNTQ   LOOKING FOR SENT STATUS                      
         BE    STSNTFD                                                          
*                                                                               
STSNTCN  DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOSKEY FIND NEXT STAUS ELM                          
*                                                                               
         B     STSNTLP                                                          
*                                                                               
STSNTFD  DS    0H                  RETURN SENT DATE AND TIME                    
*                                                                               
         GOTOR DATCON,DMCB,(3,WIOSDATE),(17,WORK)  CONVERT TO MMMDD/YY          
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#SENDDT),    X        
               ('LD_CHARQ',WORK),(8,0)                                          
*                                                                               
         LA    R2,FLDH             POINT TO WORK SCREEN FIELD                   
         XC    FLDH,FLDH           INIT DUMMY HEADER                            
         XC    FLD,FLD             AND FIELD ITSELF                             
         MVI   FLDH,L'FLDH+L'FLD SET DUMMY FIELD LENGTH                         
*                                                                               
         GOTOR DISTIM,DMCB,WIOSTIME   DISPLAY TIME                              
*                                                                               
         BRAS  RE,SETLEN           SET DATA LENGTH                              
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#SENDTM),    X        
               ('LD_CHARQ',FLD),(FLDH+5,0)                                      
*                                                                               
STSNTDN  DS    0H                                                               
*                                                                               
STHDRX   DS    0H                                                               
*                                                                               
*        STATUS DATA                                                            
*                                                                               
         TM    WRKDATTP,WRKSTADQ   IF STATUS DATA WANTED                        
         BNO   STSTAX                                                           
*                                                                               
*        FIND STATUS ELEMENT                                                    
*                                                                               
         XC    SVSTAELM,SVSTAELM   INIT SAVED STATUS ELEMENT                    
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT          ESTABLISH DETAIL ELEMENT KEY                 
         USING WIOSKEY,R6                                                       
*                                                                               
         MVI   WIOSKCDE,WIOSKIDQ   SET ELEMENT ID                               
         MVI   WIOSKLEN,1          ANY STATUS ELEMENT                           
*                                                                               
         GOTO1 GETELM,DMCB,ELEMENT FIND FIRST STATUS ELEMENT                    
         BNZ   STSTAX              NO STATUS FOUND                              
*                                                                               
STSTALP  DS    0H                                                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOSKCDE,WIOSKIDQ   DONE IF NOT A STATUS ELEMENT                 
         BNE   STSTADN                                                          
*                                                                               
         CLI   LNKSTATP,LNKALLSQ   SEND IF ALL STATUS WANTED                    
         BNE   *+12                                                             
         BRAS  RE,STSTASND                                                      
         B     STSTACN                                                          
*                                                                               
         CLI   LNKSTATP,LNKCURSQ   IF CURRENT STATUS                            
         BNE   *+14                                                             
         CLC   WIOSSTAT,QSTAT         SKIP IF NOT EQUAL HEADER STATUS           
         BNE   STSTACN                                                          
*                                                                               
         SR    RF,RF               ELSE SAVE STATUS ELEMENT                     
         IC    RF,WIOSKLEN         GET LENGTH                                   
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVSTAELM(0),WIOSKEY                                              
*                                                                               
STSTACN  DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOSKEY FIND NEXT STATUS ELM                         
*                                                                               
         B     STSTALP                                                          
*                                                                               
STSTADN  DS    0H                                                               
*                                                                               
         CLI   LNKSTATP,LNKCURSQ    IF CURRENT STATUS WANTED                    
         BNE   STSTAX                                                           
*                                                                               
         OC    SVSTAELM,SVSTAELM       SKIP IF NO STATUS FOUND                  
         BZ    STSTAX                                                           
*                                                                               
         LA    R6,SVSTAELM             POINT TO SAVED STATUS                    
         BRAS  RE,STSTASND             SEND SAVED STATUS                        
*                                                                               
STSTAX   DS    0H                                                               
*                                                                               
*        VENDOR DATA                                                            
*                                IF VENDOR, E-MAIL DATA WANTED                  
         TM    WRKDATTP,WRKVDRDQ+WRKEMLDQ                                       
         BZ    STVDRX                                                           
*                                                                               
*        FIND LATEST GROUP NUMBER                                               
*                                                                               
         XC    SVHDRELM,SVHDRELM   INIT SAVEAREA                                
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT          ESTABLISH DETAIL ELEMENT KEY                 
         USING WIOFKEY,R6                                                       
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET FAX/EMAIL ELEMENT ID                     
         MVI   WIOFKLEN,1          LOOKING FOR ANY FAX ELEMENT                  
*                                                                               
         GOTO1 GETELM,DMCB,ELEMENT FIND FAX ELEMENT                             
         BNZ   STGRPDN             NO FAX ELEMENT FOUND                         
*                                                                               
STGRPLP  DS    0H                                                               
*                                                                               
         CLI   MINERR,0            DONE ON ERRORS                               
         BNE   STGRPDN                                                          
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOFKCDE,WIOFKIDQ   DONE IF NOT A FAX/E-MAIL ELM                 
         BNE   STGRPDN                                                          
*                                                                               
         CLI   WIOFKTYP,WIOFKHDQ   SKIP UNLESS A GROUP HEADER ELEMENT           
         BNE   STGRPCN                                                          
*                                                                               
         MVC   SVHDRELM,WIOFKEY    SAVE FAX HEADER ELEMENT                      
*                                                                               
STGRPCN  DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOFKEY FIND NEXT ELM IN MINIO SETGROUP              
*                                                                               
         B     STGRPLP                                                          
*                                                                               
STGRPDN  DS    0H                                                               
*                                                                               
         OC    SVHDRELM,SVHDRELM   NO DATA IF NO GROUP HEADER ELEMENT           
         BZ    STVDRX                                                           
*                                                                               
         MVC   ELEMENT,SVHDRELM    COPY SAVED CURRENT GROUP HEADER              
*                                                                               
         GOTO1 GETELM,DMCB,ELEMENT  RE-POINT TO GROUP HEADER ELEMENT            
         BNZ   STVDRX              NOT FOUND                                    
*                                                                               
*        RETURN SENDING DATA                                                    
*                                                                               
STVDRLP  DS    0H                                                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOFKCDE,WIOFKIDQ   DONE IF NOT FAX/E-MAIL ELM                   
         BNE   STVDRDN                                                          
*                                                                               
         CLC   WIOFKGP#,WIOFKGP#-WIOFKEY+ELEMENT    DONE ON CHG IN GP#          
         BNE   STVDRDN                                                          
*                                                                               
STVDREM  DS    0H                  E-MAIL ELEMENT                               
*                                                                               
         CLI   WIOFKTYP,WIOFKEMQ   IF E-MAIL ELEMENT                            
         BNE   STVDREMN                                                         
*                                                                               
         TM    WRKDATTP,WRKEMLDQ      IF E-MAIL DATA WANTED                     
         BNO   STVDREMN                                                         
*                                                                               
         BRAS  RE,PUTEML                 SEND E-MAIL DATA                       
*                                                                               
         B     STVDRCN                                                          
*                                                                               
STVDREMN DS    0H                                                               
*                                                                               
STVDREC  DS    0H                  E-MAIL COMMENT ELEMENT                       
*                                                                               
         CLI   WIOFKTYP,WIOFKECQ   IF E-MAIL COMMENT ELEMENT                    
         BNE   STVDRECN                                                         
*                                                                               
         TM    WRKDATTP,WRKEMLDQ      IF E-MAIL DATA WANTED                     
         BNO   STVDRECN                                                         
*                                                                               
         BRAS  RE,PUTECOM                SEND E-MAIL COMMENT                    
*                                                                               
         B     STVDRCN                                                          
*                                                                               
STVDRECN DS    0H                                                               
*                                                                               
STVDRFXL DS    0H                  FAX/E-MAIL ELEMENT                           
*                                                                               
         CLI   WIOFKTYP,WIOFKFXQ   IF FAX/E-MAIL ELEMENT                        
         BNE   STVDRFXN                                                         
*                                                                               
         CLI   WIOFKFTP,WIOFKFAQ   SKIP IF NOT ADDRESS ELEMENT                  
         BNE   STVDRCN                                                          
*                                                                               
         TM    WRKDATTP,WRKVDRDQ      IF VENDOR DATA WANTED                     
         BNO   STVDRFXN                                                         
*                                                                               
         BRAS  RE,PUTFAX                 SEND FAX DATA                          
*                                                                               
         B     STVDRCN                                                          
*                                                                               
STVDRFXN DS    0H                                                               
*                                                                               
STVDRCN  DS    0H                                                               
*                                                                               
         MVC   ELEMENT,WIOFKEY     SAVE CURRENT ELEMENT                         
*                                                                               
         GOTOR NXTELM,DMCB,ELEMENT   READ NEXT ELEMENT                          
*                                                                               
         B     STVDRLP                                                          
*                                                                               
STVDRDN  DS    0H                                                               
*                                                                               
STVDRCMX DS    0H                                                               
*                                                                               
STVDRX   DS    0H                                                               
*                                                                               
*        VENDOR COMMENTS                                                        
*              REPORTS FOR ALL GROUP NUMBERS                                    
*                                IF VENDOR DATA WANTED                          
         TM    WRKDATTP,WRKVDRDQ                                                
         BZ    STVDCX                                                           
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT          ESTABLISH DETAIL ELEMENT KEY                 
         USING WIOFKEY,R6                                                       
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET FAX/EMAIL ELEMENT ID                     
         MVI   WIOFKLEN,1          LOOKING FOR ANY FAX ELEMENT                  
*                                                                               
         GOTO1 GETELM,DMCB,ELEMENT FIND VENDOR COMMENT ELM                      
         BNZ   STVDCDN             NO VENDOR COMMENTS FOUND                     
*                                                                               
STVDCLP  DS    0H                                                               
*                                                                               
         CLI   MINERR,0            DONE ON ERRORS                               
         BNE   STVDCDN                                                          
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOFKCDE,WIOFKIDQ   DONE IF NOT A FAX/E-MAIL ELM                 
         BNE   STVDCDN                                                          
*                                                                               
         CLI   WIOFKTYP,WIOFKVCQ   SKIP UNLESS A VENDOR COMMENT ELM             
         BNE   STVDCCN                                                          
*                                                                               
         BRAS  RE,PUTVCOM                SEND VENDOR COMMENT                    
*                                                                               
STVDCCN  DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOFKEY FIND NEXT ELM IN MINIO SETGROUP              
*                                                                               
         B     STVDCLP                                                          
*                                                                               
STVDCDN  DS    0H                                                               
*                                                                               
STVDCX   DS    0H                                                               
*                                                                               
*        URL DATA                                                               
*                                IF URL DATA WANTED                             
         TM    WRKDATTP,WRKURLDQ                                                
         BZ    STURLX                                                           
*                                                                               
*        FIND URL ELEMENTS                                                      
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT          ESTABLISH DETAIL ELEMENT KEY                 
         USING WIOUKEY,R6                                                       
*                                                                               
         MVI   WIOUKCDE,WIOUKIDQ   SET URL ELEMENT ID                           
         MVI   WIOUKLEN,1          LOOKING FOR ANY URL ELEMENT                  
*                                                                               
         GOTO1 GETELM,DMCB,ELEMENT FIND URL ELM                                 
         BNZ   STURLDN             NO URLS FOUND                                
*                                                                               
STURLLP  DS    0H                                                               
*                                                                               
         CLI   MINERR,0            DONE ON ERRORS                               
         BNE   STURLDN                                                          
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOUKCDE,WIOUKIDQ   DONE IF NOT A URL ELM                        
         BNE   STURLDN                                                          
*                                                                               
         BRAS  RE,PUTURL                 SEND URL DATA                          
*                                                                               
STURLCN  DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOUKEY FIND NEXT ELM IN MINIO SETGROUP              
*                                                                               
         B     STURLLP                                                          
*                                                                               
STURLDN  DS    0H                                                               
*                                                                               
STURLX   DS    0H                                                               
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
         CLI   MINERR,0            MUST SUCCEED                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
STX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST - STSTASND'                          
***********************************************************************         
*                                                                     *         
*        RETURN STATUS DATA TO ADBUYER                                *         
*        R6==> STATUS ELEMENT                                         *         
*        R3==) LINKIO AREA                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
STSTASND NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         USING WIOSKEY,R6          ESTABLISH STATUS ELEMENT                     
         USING LIOBD,R3            ESTABLISH LINKIO AREA                        
*                                                                               
*        SEND STATUS TO ADBUYER                                                 
*                                                                               
         LA    R2,FLDH             POINT TO WORK SCREEN FIELD                   
         XC    FLDH,FLDH           INIT DUMMY HEADER                            
         XC    FLD,FLD             AND FIELD ITSELF                             
         MVI   FLDH,L'FLDH+L'FLD SET DUMMY FIELD LENGTH                         
*                                                                               
         MVC   QSTAT,WIOSSTAT      SET STATUS                                   
*                                                                               
         GOTOR DISSTA               DISPLAY STATUS                              
*                                                                               
         BRAS  RE,SETLEN           SET DATA LENGTH                              
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#IORSTA),    X        
               ('LD_CHARQ',FLD),(FLDH+5,0)                                      
*                                                                               
*        SEND STATUS DATE TO ADBUYER                                            
*                                                                               
         GOTOR DATCON,DMCB,(3,WIOSDATE),(17,WORK)  CONVERT TO MMMDD/YY          
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#STATDT),    X        
               ('LD_CHARQ',WORK),(8,0)                                          
*                                                                               
*        SEND STATUS TIME TO ADBUYER                                            
*                                                                               
         LA    R2,FLDH             POINT TO WORK SCREEN FIELD                   
         XC    FLDH,FLDH           INIT DUMMY HEADER                            
         XC    FLD,FLD             AND FIELD ITSELF                             
         MVI   FLDH,L'FLDH+L'FLD SET DUMMY FIELD LENGTH                         
*                                                                               
         GOTOR DISTIM,DMCB,WIOSTIME   DISPLAY TIME                              
*                                                                               
         BRAS  RE,SETLEN           SET DATA LENGTH                              
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#STATTM),    X        
               ('LD_CHARQ',FLD),(FLDH+5,0)                                      
*                                                                               
STSTASNX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST - PUTEML'                            
***********************************************************************         
*                                                                     *         
*        RETURN E-MAIL ADDRESS TO ADBUYER                             *         
*                                                                     *         
*NTRY    R6==> E-MAIL ELEMENT                                         *         
*        R3==> LINKIO AREA                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PUTEML   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         USING WIOFKEY,R6          ESTABLISH E-MAIL ELEMENT                     
         USING LIOBD,R3            ESTABLISH LINKIO AREA                        
*                                                                               
*        SEND E-MAIL ADDRESS TO ADBUYER                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,WIOFKLEN         ELEMENT LENGTH                               
*                                                                               
         LA    R1,WIOFKEY                                                       
         AR    R1,R0               LAST BYTE OF ADDRESS                         
         SHI   R1,1                                                             
*                                                                               
         SHI   R0,WIOFEMLL         E-MAIL ADDRESS LENGTH                        
*                                  FIND END OF ADDRESS                          
         CLI   0(R1),C' '          FIND LAST NON-BLANK CHARACTER                
         BH    *+16                                                             
         SHI   R1,1                BACK UP A BYTE                               
         BCT   R0,*-12                                                          
         B     PUTEMLX             NO ADDRESS AVAILABLE                         
*                                  R0 HAS ADDRESS LENGTH                        
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#E_MAIL),    X        
               ('LD_CHARQ',WIOFEML),((R0),0)                                    
*                                                                               
PUTEMLX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST - PUTECOM'                           
***********************************************************************         
*                                                                     *         
*        RETURN E-MAIL COMMENTS TO ADBUYER                            *         
*                                                                     *         
*NTRY    R6==> E-MAIL COMMENT ELEMENT                                 *         
*        R3==> LINKIO AREA                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PUTECOM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         USING WIOFKEY,R6          ESTABLISH E-MAIL ELEMENT                     
         USING LIOBD,R3            ESTABLISH LINKIO AREA                        
*                                                                               
*        SEND E-MAIL COMMENT TO ADBUYER                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,WIOFKLEN         GET ELEMENT LENGTH                           
         SHI   R0,WIOFCOML         SUBTRACT HEADER LENGTH                       
         BNP   PUTECOMX            NO COMMENT TO SEND                           
*                                                                               
*                                  R0 HAS COMMENT LENGTH                        
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#EMLCOM),    X        
               ('LD_CHARQ',WIOFCOMM),((R0),0)                                   
*                                                                               
PUTECOMX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST - PUTVCOM'                           
***********************************************************************         
*                                                                     *         
*        RETURN VENDOR COMMENTS TO ADBUYER                            *         
*                                                                     *         
*NTRY    R6==> VENDOR  COMMENT ELEMENT                                *         
*        R3==> LINKIO AREA                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PUTVCOM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         USING WIOFKEY,R6          ESTABLISH E-MAIL ELEMENT                     
         USING LIOBD,R3            ESTABLISH LINKIO AREA                        
*                                                                               
*        SEND VENDOR COMMENT TO ADBUYER                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,WIOFKLEN         GET ELEMENT LENGTH                           
         SHI   R0,WIOFCOML         SUBTRACT HEADER LENGTH                       
         BNP   PUTVCOMX            NO COMMENT TO SEND                           
*                                                                               
*                                  R0 HAS COMMENT LENGTH                        
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#COMMNT),    X        
               ('LD_CHARQ',WIOFCOMM),((R0),0)                                   
*                                                                               
PUTVCOMX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST - PUTURL'                            
***********************************************************************         
*                                                                     *         
*        RETURN URL    DATA TO ADBUYER                                *         
*                                                                     *         
*NTRY    R6==> URL ELEMENT                                            *         
*        R3==> LINKIO AREA                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PUTURL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         USING WIOURLD,R6          ESTABLISH E-MAIL ELEMENT                     
         USING LIOBD,R3            ESTABLISH LINKIO AREA                        
*                                                                               
*        SEND URL TO ADBUYER                                                    
*                                                                               
         SR    R0,R0                                                            
         IC    R0,WIOUKLEN         GET ELEMENT LENGTH                           
         SHI   R0,WIOURLL          SUBTRACT HEADER LENGTH                       
         BNP   PUTURLX             NO URL TO SEND                               
*                                  R0 HAS URL LENGTH                            
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#WEBURL),    X        
               ('LD_CHARQ',WIOUKID),((R0),0)                                    
*                                                                               
*        SEND URL TYPE TO ADBUYER                                               
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#URLTYP),    X        
               ('LD_CHARQ',WIOUTYP),(L'WIOUTYP,0)                               
*                                                                               
*        SEND URL STYLE TO ADBUYER                                              
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#URLSTL),    X        
               ('LD_CHARQ',WIOUSTL),(L'WIOUSTL,0)                               
*                                                                               
*        SEND URL DELIVERY METHOD TO ADBUYER                                    
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#URLDLM),    X        
               ('LD_CHARQ',WIOUDLM),(L'WIOUDLM,0)                               
*                                                                               
PUTURLX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST - PUTFAX'                            
***********************************************************************         
*                                                                     *         
*        RETURN FAX    DATA TO ADBUYER                                *         
*                                                                     *         
*NTRY    R6==> FAX ELEMENT                                            *         
*        R3==> LINKIO AREA                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PUTFAX   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         USING WIOFKEY,R6          ESTABLISH E-MAIL ELEMENT                     
         USING LIOBD,R3            ESTABLISH LINKIO AREA                        
*                                                                               
*        SEND RECIPIENT'S NAME TO ADBUYER                                       
*                                                                               
         LA    R0,L'WIOFXNAM       MAX LENGTH OF NAME                           
         LA    R1,WIOFXNAM+L'WIOFXNAM-1 LAST BYTE OF NAME                       
*                                  FIND END OF NAME                             
         CLI   0(R1),C' '          FIND LAST NON-BLANK CHARACTER                
         BH    *+16                                                             
         SHI   R1,1                BACK UP A BYTE                               
         BCT   R0,*-12                                                          
         B     PUTFAX10            NO NAME AVAILABLE                            
*                                  R0 HAS ADDRESS LENGTH                        
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#RECPNM),    X        
               ('LD_CHARQ',WIOFXNAM),((R0),0)                                   
*                                                                               
*        SEND FYI OR PRIMARY OR AGENCY TYPE                                     
*                                                                               
         MVI   WORK,0              DEFAULT TO VENDOR PRIMARY TYPE               
*                                                                               
         CLI   WIOFXTYP,C'F'       CHANGE TO VENDOR FYI IF FYI TYPE             
         BNE   *+8                                                              
         MVI   WORK,C'F'                                                        
*                                                                               
         CLI   WIOFXAGV,C'A'       CHANGE IF AGENCY ADDRESS                     
         BNE   *+8                                                              
         MVI   WORK,C'A'                                                        
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#TYPE_2),    X        
               ('LD_CHARQ',WORK),(L'WIOFXTYP,0)                                 
*                                                                               
*        SEND FAX OR E-MAIL TYPE                                                
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#TYPE_1),    X        
               ('LD_CHARQ',WIOFXTP1),(L'WIOFXTP1,0)                             
PUTFAX10 DS    0H                                                               
*                                                                               
*        SEND RECIPIENT'S NUMBER/ADDRESS TO ADBUYER                             
*                                                                               
         CLI   WIOFXTP1,C'E'       IF E-MAIL TYPE ADDRESS                       
         BNE   PUTFAX40                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,WIOFKLEN         GET ELEMENT LENGTH                           
         SHI   R0,WIOFFAXL         SUBTRACT HEADER LENGTH                       
         BNP   PUTFAX50            NO ADDRESS TO SEND                           
*                                  R0 HAS ADDRESS LENGTH                        
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ADRFAX),    X        
               ('LD_CHARQ',WIOFFEML),((R0),0)                                   
*                                                                               
         B     PUTFAX50                                                         
*                                                                               
PUTFAX40 DS    0H                  FAX NUMBER                                   
*                                                                               
         LA    R0,L'WIOFX#         MAX LENGTH OF NUMBER                         
         LA    R1,WIOFX#+L'WIOFX#-1 LAST BYTE OF NUMBER                         
*                                  FIND END OF NAME                             
         CLI   0(R1),C' '          FIND LAST NON-BLANK CHARACTER                
         BH    *+16                                                             
         SHI   R1,1                BACK UP A BYTE                               
         BCT   R0,*-12                                                          
         B     PUTFAX50            NO NAME AVAILABLE                            
*                                  R0 HAS ADDRESS LENGTH                        
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ADRFAX),    X        
               ('LD_CHARQ',WIOFX#),((R0),0)                                     
*                                                                               
PUTFAX50 DS    0H                                                               
*                                                                               
*        SEND SUPPRESS COSTS INDICATOR TO ADBUYER                               
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#SUPCOS),    X        
               ('LD_CHARQ',WIOFXSCS),(L'WIOFXSCS,0)                             
*                                                                               
*        SEND STATUS TO ADBUYER  OR PRIMARY TYPE                                
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#WIOSTA),    X        
               ('LD_CHARQ',WIOFXSTA),(L'WIOFXSTA,0)                             
*                                                                               
PUTFAXX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PR       DS    0H                  PRINT RECORDS                                
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
PRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
SELMSG01 DC    C'RECORD DISPLAYED - HIT PF12 TO RETURN OR NEXT SEL'             
*                                                                               
PF12TXT  DC    C'PF12=RETURN/NEXTSEL'                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T41E20 - FAX MAINT/LIST - WKRDATD'                              
***********************************************************************         
*                                                                     *         
*        WORKER RECORD DATA    DSECT                                  *         
*                                                                     *         
***********************************************************************         
WKRDATD  DSECT                     WORKER RECORD DATA                           
WKDTRID  DS    XL1                 RECORD ID                                    
WKDTRLEN DS    XL2                 RECORD LENGTH                                
WKDTMPCD DS    XL2                 MAP CODE                                     
WKDTTYPE DS    XL1                 DATA TYPE                                    
WKDTHDLQ EQU   *-WKRDATD           HEADER LENGTH                                
WKDTDATA DS    0C                  DATA                                         
*                                                                               
         TITLE 'T41E30 - STATUS MAINT/LIST - LISTLIND'                          
***********************************************************************         
*                                                                     *         
*        DSECT FOR A LIST SCREEN LINE                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LISTLIND DSECT                                                                  
LISTLIN  DS    0XL(L'LSTLIN1)      LIST SCREEN LINE                             
LSSSQN   DS    CL3                 SEQ NUMBER                                   
         DS    CL2                                                              
LSSTA    DS    CL15                STATUS                                       
         DS    CL2                                                              
LSSDTE   DS    CL8                 STATUS   DATE                                
         DS    CL2                                                              
LSSTIM   DS    CL8                 STATUS   TIME                                
         DS    CL2                                                              
LSSNAM   DS    CL25                SENDER'S NAME                                
         DS    CL2                                                              
         PRINT OFF                                                              
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPWIOFFD                                                       
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPWIOFBD          STATUS MAINT SCREEN                          
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPWIOF0D          DETAIL LIST SCREEN                           
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPWIOWRKD                                                      
         ORG   SYSSPARE            WORKING AREA                                 
*                                                                               
RELO20   DS    F                   RELOACTION FACTOR                            
*                                                                               
NXTIOKYA DS    A                   A(NEXT IOKEY FROM LINK)                      
NXTFLTA  DS    A                   A(NEXT SET OF FILTERS )                      
*                                                                               
CHGSWTCH DS    CL1                 C'Y' - A KEY FIELD HAS BEEN CHGD             
CHGMED   EQU   X'80'               AGENCY CHANGED                               
CHGCLT   EQU   X'40'               CLIENT CHANGED                               
CHGPUB   EQU   X'20'               PUB CHANGED                                  
CHGIO#   EQU   X'10'               INSORD     NUMBER CHANGED                    
CHGRV#   EQU   X'08'               REVISION   NUMBER CHANGED                    
CHGSSQN  EQU   X'04'               STATUS SEQ NUMBER CHANGED                    
*                                                                               
DATAFLDS DS    XL1                 KEY FIELDS WITH DATA                         
DFLCLTQ  EQU   X'80'               CLIENT                                       
DFLPUBQ  EQU   X'40'               PUB                                          
DFLIO#Q  EQU   X'20'               IO#                                          
DFLRV#Q  EQU   X'10'               REVISION #                                   
DFLPERQ  EQU   X'08'               PERIOD                                       
DFLSSQNQ EQU   X'04'               STATUS SEQ NUMBER                            
*                                                                               
SVCLT    DS    CL3                 CLIENT CODE SAVEAREA                         
SVDCL    DS    CL3                 DETAIL CLIENT CODE SAVEAREA                  
SVCLTNM  DS    CL20                CLIENT NAME SAVEAREA                         
SVPRD    DS    CL3                 PRODUCT CODE SAVEAREA                        
SVDPR    DS    CL3                 PRODUCT CODE SAVEAREA                        
SVPRDNM  DS    CL20                PRODUCT NAME SAVEAREA                        
SVEST    DS    CL3                 ESTIMATE CODE SAVEAREA                       
SVDES    DS    CL3                 ESTIMATE CODE SAVEAREA                       
SVESTNM  DS    CL20                ESTIMATE NAME SAVEAREA                       
SVPUB    DS    XL6                 PUB NUMBER SAVEAREA                          
SVPUBNM  DS    CL20                PUB NAME   SAVEAREA                          
*                                                                               
SVHDRELM DS    XL256               HEADER ELEMENT SAVEAREA                      
SVFAXELM DS    XL256               FAX/E-MAIL SAVEARE                           
SVSTAELM DS    XL256               STATUS ELEMENT SAVEAREA                      
SVIOKEY  DS    XL(L'WIOKEY)        INSORD KEY SAVEAREA FOR LIST                 
SVWIO1KY DS    XL(L'WIO1KEY)       INSORD PASSIVE SAVEAREA                      
SVLSTKEY DS    XL(L'WIOSKEY)       LIST KEY SAVEAREA                            
*                                                                               
SVLSTSST DS    16XL(L'WIOSKEY)     DETAIL KEYS FOR LIST                         
SVLSTLNQ EQU   *-SVLSTSST          TABLE LENGTH                                 
*                                                                               
SVDATE   DS    XL3                 TODAY'S DATE                                 
SVTIME   DS    XL3                 TIME - HMS - BINARY                          
*                                                                               
LRLASTSW DS    XL1                 C'Y' - END OF DETAILS                        
*                                                                               
ACTSW    DS    XL1                 C'N' - NEW ACTIVITY ELEMENT                  
*                                  C'O' - OLD ACTIVITY ELEMENT                  
*                                                                               
LNKSTSW  DS    XL1                 STATUS PROCESSTING SWITCH                    
*                                    C'1' - FIRST IO KEY FROM LINK              
*                                    C'2' - SECOND OR LATER IO KEY              
*                                    C'L' - LAST IOKEY IN REQUEST               
*                                                                               
LNKFLSW  DS    XL1                 FILTER PROCESSTING SWITCH                    
*                                    C'1' - FIRST FILTER FROM LINK              
*                                    C'2' - SECOND OR LATER FILTER              
*                                    C'L' - LAST FILTER IN REQUEST              
*                                                                               
LNKREPCD DS    XL2                 LINK REPLY RECORD CODE                       
*                                                                               
         ORG                                                                    
*                                                                               
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
*                                                                               
*PRGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE         PRINT SYSTEM RECORD LAYOUTS                  
         PRINT ON                                                               
** DDPERVALD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD         PERVAL CONTROL BLOCKALD                      
         PRINT ON                                                               
*DDMINBLK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMINBLK          MINIO CONTROL BLOCK                          
         PRINT ON                                                               
*DMPRTQL                                                                        
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL           PRINT QUEUE PRINT LINE                       
         PRINT ON                                                               
*PPERREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPERREQUS         PRINT SYSTEM RECORD LAYOUTS                  
         EJECT                                                                  
         PRINT ON                                                               
*PPMAPEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPMAPEQUS         PRINT SYSTEM MAP CODE EQUATES                
         EJECT                                                                  
         PRINT ON                                                               
*DDLINKD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDLINKD           LINKIO DSECT                                 
         EJECT                                                                  
         PRINT ON                                                               
*DDLINKIOD                         LINKIO CONTROL BLOCK                         
LIOBD    DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDLINKIOD                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE PTRACLTPP         CLT TRAFFIC OFFICE CODE PASSIVE PTR          
         EJECT                                                                  
*                                                                               
       ++INCLUDE POFFCLTPP         CLT OFFICE CODE PASSIVE PTR                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDFLDHDR          FIELD INDICATOR EQUATES                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDPSTBLK          BLOCK FOR PST VALIDATION CALL                
         EJECT                                                                  
*                                                                               
       ++INCLUDE FAFACTS           MASTER SYS INFO BLOCK                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTGENFILE         DSECT FOR CONTROL FILE RECORDS               
         EJECT                                                                  
*                                                                               
       ++INCLUDE ACGENFILE         DSECT FOR OFFICE RECORDS                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLVXCTLD        GLOBBER TRANSFER CONTROLS                    
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLOBEQUS        DSECT FOR GLOBBER                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSCANBLKD        DSECT FOR SCANNER                            
F_SMAXQ  EQU   5                   MAX NUM OF FILTER SCANNER ENTRIES            
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018PPWIO30   02/20/15'                                      
         END                                                                    
