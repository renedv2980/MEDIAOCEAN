*          DATA SET PPWIO10    AT LEVEL 008 AS OF 12/17/07                      
*PHASE T41E10A                                                                  
*                                                                               
         TITLE 'T41E10 - WEB IO HEADER MAINT/LIST'                              
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               CHANGE LOG                                            *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 2                                                                
*                                                                               
*KWAN    SEP/05 DELETE MAT= REPEAT PASSIVE KEYS WHEN DELETING EIO               
*                                                                               
*BOBY    JUN/04 BIG BANG                                                        
*                                                                               
         TITLE 'T41E10 - WEB IO HEADER MAINT/LIST'                              
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               T41E10 - WEB IO HEADER MAINT/LIST      *                        
*                                                                     *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41E00 (WIO CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, CHG, DISP, SEL, LIST, REP, DEL, RES     *         
*                                                                     *         
*  INPUTS       SCREEN T41EFD (MAINTENANCE)                           *         
*               SCREEN T41EFE (LIST)                                  *         
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
         TITLE 'T41E10 - WEB IO HEADER MAINT/LIST '                             
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T41E10   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41E10,RR=RE                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         L     R9,ASYSD                                                         
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         ST    RE,RELO10                                                        
*                                                                               
         GOTOR MININIT             INIT MINIO BLOCK                             
*                                                                               
         MVC   QRECORD,=CL8'WEB IO'                                             
*                                                                               
         TITLE 'T41E10 - WEB IO HEADER MAINT/LIST - CKMODE'                     
***********************************************************************         
*                                                                     *         
*        DETERMINE CALLING MODE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CKMODE   DS    0H                                                               
*                                                                               
*                                                                               
         CLI   MODE,VALKEY         VALKEY?                                      
         BNE   *+12                                                             
         BRAS  RE,VK                                                            
         B     CKMODEX                                                          
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
         MVC   PAGEDAS+60(4),LASTLIST                                           
*                                                                               
         B     CKMODEX                                                          
*                                                                               
CKMDPFKX DS    0H                                                               
*                                                                               
         CLI   MODE,VALREC         VALREC?                                      
         BNE   CKMDVRN                                                          
*                                                                               
         CLI   ACTNUM,ACTABDEL     IF DELETING IOS                              
         BNE   CKMDVR30                                                         
*                                                                               
         CLI   DDLNKSW,C'Y'        AND A LINK CALL                              
         BNE   CKMDVR30                                                         
*                                                                               
         BRAS  RE,ABDEL               DELETE SEVERAL IOS                        
                                                                                
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
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   *+12                                                             
         BRAS  RE,LR                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,RECDEL         RECDEL?                                      
         BNE   *+12                                                             
         BRAS  RE,RDEL                                                          
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,RECREST        RESREC?                                      
         BNE   *+12                                                             
         BRAS  RE,RS                                                            
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
         DROP                                                                   
*                                                                               
         TITLE 'T41E10 - WEB IO HEADER MAINT/LIST - VK'                         
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
         MVI   CHGSWTCH,0          INITIALIZE CHANGE SWITCH                     
         MVI   DATAFLDS,0          INITIALIZE FIELDS WITH DATA                  
*                                                                               
         MVC   SVKEY,KEY           SAVE CURRENT PRTDIR KEY                      
*                                                                               
         GOTOR GETSCH              READ IN SCHEMA RECORD                        
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKL                                                              
*                                                                               
*        HANDLE MUTIPLE DELETES                                                 
*                                                                               
VKABDEL  DS    0H                                                               
*                                                                               
         CLI   DDLNKSW,C'Y'        IF LINK CALL                                 
         BNE   VKABDELX                                                         
*                                                                               
         CLI   ACTNUM,ACTABDEL     IF  IO DELETE                                
         BNE   VKABDELX                                                         
*                                                                               
         LHI   RF,E#IODLRP         SET APPROPRIATE RETURN CODE                  
         STCM  RF,3,LNKREPCD       SET LINK REPLY CODE                          
*                                                                               
         BRAS  RE,FILLKEY             FILL IN KEY WITH NEXT IO KEY              
*                                                                               
         CLI   LNKSTSW,C'L'        DONE IF END OF LIST OF KEYS                  
         BE    VKX                                                              
*                                                                               
VKABDELX DS    0H                                                               
*                                                                               
*        VALIDATE MEDIA                                                         
*                                                                               
VKMED    DS    0H                                                               
*                                                                               
         LA    R2,HDRMEDH          POINT TO MEDIA FIELD                         
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         CLI   FLDILEN,0           MEDIA IS REQUIRED                            
         BE    VKMEDER                                                          
*                                                                               
         GOTOR VALMED              VALIDATE MEDIA                               
*                                                                               
VKMEDX   DS    0H                                                               
*                                                                               
*        VALIDATE CLIENT                                                        
*                                                                               
VKCLT    DS    0H                  VALIDATE CLIENT                              
*                                                                               
         LA    R2,HDRCLTH          POINT TO CLIENT FIELD                        
*                                                                               
         CLI   FLDILEN,0           CLIENT IS REQUIRED                           
         BE    VKCLTER                                                          
*                                                                               
         GOTOR VALCLT              VALIDATE CLIENT                              
*                                                                               
VKCLTX   DS    0H                                                               
*                                                                               
*        VALIDATE PUB                                                           
*                                                                               
VKPUB    DS    0H                                                               
*                                                                               
         LA    R2,HDRPUBH          POINT TO PUB FIELD                           
*                                                                               
         CLI   FLDILEN,0           PUB IS REQUIRED                              
         BE    VKPUBER                                                          
*                                                                               
         GOTOR VALPUB              VALIDATE PUB                                 
*                                                                               
VKPUBX   DS    0H                                                               
*                                                                               
*        VALIDATE IO#                                                           
*                                                                               
VKIO#    DS    0H                                                               
*                                                                               
         LA    R2,HDRIO#H          POINT TO WEB IO FIELD                        
*                                                                               
         XC    QIO#,QIO#           INIT IO#                                     
*                                                                               
         CLI   FLDILEN,0           OKAY IF IO# MISSING                          
         BE    VKIO#X                                                           
*                                                                               
         GOTOR VALIO#             VALIDATE WEB IO #                             
*                                                                               
         OI    DATAFLDS,DFLIO#Q    IO#    ENTERED                               
*                                                                               
VKIO#X   DS    0H                                                               
*                                                                               
*        VALIDATE REVISION #                                                    
*                                                                               
VKRV#    DS    0H                                                               
*                                                                               
         LA    R2,HDRREV#H         POINT TO REVISION FIELD                      
*                                                                               
         XC    QREV#,QREV#         INIT REVISION NUMBER                         
*                                                                               
         CLI   FLDILEN,0           OKAY IF IO# MISSING                          
         BE    VKRV#X                                                           
*                                                                               
         GOTOR VALRV#              VALIDATE REVISION #                          
*                                                                               
         OI    DATAFLDS,DFLRV#Q    RV#    ENTERED                               
*                                                                               
VKRV#X   DS    0H                                                               
*                                                                               
* VALIDATE PERIOD                                                               
*                                                                               
VKPER    DS    0H                                                               
*                                                                               
         LA    R2,HDRPERH            PERIOD                                     
*                                                                               
         CLI   FLDILEN,0             PERIOD IS OPTONAL                          
         BE    VKPERX                                                           
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
*        VALIDATE STEWARDSHIP                                                   
*                                                                               
VKSTW    DS    0H                                                               
*                                                                               
         LA    R2,HDRSTEWH           STEWARD BUYS ONLY                          
         MVI   QSTEW,0             INIT WORKAREA SAVEAREA                       
*                                                                               
         CLI   FLDILEN,0             STEWARD IS OPTONAL                         
         BE    VKSTWX                                                           
*                                                                               
         CLI   FLDDATA,C'S'        'S' ONLY ACCEPTABLE VALUE                    
         BNE   VKSTWER                                                          
*                                                                               
         OI    DATAFLDS,DFLSTWQ    STEWARD ENTERED                              
         MVI   QSTEW,C'S'          SAVE INPUT                                   
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OLD DATA                               
*                                                                               
         MVC   FLDDATA(L'QSTEW),QSTEW                                           
*                                                                               
VKSTWX   DS    0H                                                               
*                                                                               
*        ALL KEYFIELDS VALIDATED                                                
*                                                                               
*        ANALYZE WHICH FIELDS ENTERED                                           
*                                                                               
         TM    DATAFLDS,DFLIO#Q+DFLRV#Q IF IO# AND REVISION GIVEN               
         BO    VKKEY                       GO BUILD KEY                         
*                                                                               
         TM    DATAFLDS,DFLRV#Q    IF RV# WITHOUT IO#                           
         BO    VKRV#ER                ERROR                                     
*                                                                               
         TM    DATAFLDS,DFLIO#Q    IF IO# WITHOUT RV#                           
         BNO   VK100                                                            
*                                                                               
         CLI   ACTNUM,ACTDEL       ASSUME ORIGINAL IO IF DELETING               
         BE    VKKEY                                                            
         CLI   ACTNUM,ACTABDEL                                                  
         BE    VKKEY                                                            
*                                                                               
         GOTOR FNDRV#,DMCB,QSTART  FIND MOST RECENT RV#                         
         BNZ   VKIO#2ER            IO NOT ON FILE                               
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF NOT ADDING                           
         BNE   VK60                                                             
*                                                                               
*        BUMP REVISION #                                                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,QREV#            GET RV#                                      
*                                                                               
         TM    WIODCNTL-WIOKEY+QIOKEY,WIODDELQ SKIP IF DELETED (REUSE)          
         BO    *+8                                                              
         AHI   RF,1                   BUMP REVISION NUMBER                      
*                                                                               
         STC   RF,QREV#            RESET REVISION NUMBER                        
*                                                                               
VK60     DS    0H                                                               
*                                                                               
         LA    R2,HDRREV#H         POINT TO REVISION FIELD                      
*                                                                               
         GOTOR DISRV#              DISPLAY RV#                                  
*                                                                               
         B     VKKEY                                                            
*                                                                               
VK100    DS    0H                  NO IO# OR RV#                                
*                                                                               
         TM    DATAFLDS,DFLPERQ    MUST HAVE PERIOD                             
         BNO   VKPERER                                                          
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF NOT ADDING A NEW IO                  
         BNE   VK200                                                            
*                                                                               
         GOTOR NXTIO#              FIND NEXT IO#                                
*                                                                               
         LA    R2,HDRIO#H          POINT TO REVISION FIELD                      
*                                                                               
         GOTOR DISIO#              DISPLAY THE NEW IO#                          
*                                                                               
         MVI   QREV#,0             NO REVISION #                                
*                                                                               
         LA    R2,HDRREV#H         POINT TO REVISION FIELD                      
*                                                                               
         GOTOR DISRV#              DISPLAY THE NEW IO#                          
*                                                                               
         B     VKKEY                                                            
*                                                                               
VK200    DS    0H                  DISPLAY LATEST IO FOR PERIOD                 
*                                                                               
*        MUST HAVE PRODUCT AS WELL AS CLT/PUB/PERIOD                            
*                                                                               
*        VALIDATE PRODUCT                                                       
*                                                                               
         LA    R2,HDRPRDH          POINT TO PRODUCT FIELD                       
*                                                                               
         CLI   FLDILEN,0           PRODUCT IS REQUIRED                          
         BE    VKPRDER                                                          
*                                                                               
         GOTOR VALPRD              VALIDATE PRODUCT                             
*                                                                               
*        FIND IO THAT FITS                                                      
*                                                                               
         GOTOR FNDIO#,DMCB,QSTART                                               
         BNZ   VKIO#2ER            WEB IO   NOT ON FILE                         
*                                                                               
         LA    R2,HDRIO#H          POINT TO IO NUMBER FIELD                     
*                                                                               
         GOTOR DISIO#              DISPLAY IO#                                  
*                                                                               
         LA    R2,HDRREV#H         POINT TO REVISION FIELD                      
*                                                                               
         GOTOR DISRV#              DISPLAY RV#                                  
*                                                                               
         B     VKKEY                                                            
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
         OI    GENSTAT1,OKADDEL    OKAY TO ADD DELETED RECORDS                  
*                                                                               
         MVC   QIOKEY,KEY          SAVE KEY INTO QIOKEY                         
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTOR HIGH                READ MATER RECORD                            
*                                                                               
         CLC   WIOKEY,KEYSAVE      IF RECORD NOT FOUND                          
         BE    VKKEY20                                                          
*                                                                               
         CLI   ACTNUM,ACTADD          ERROR IF NOT ADDING                       
         BNE   VKIO#2ER                                                         
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE SOUGHT AFTER KEY                     
*                                                                               
VKKEY20  DS    0H                                                               
*                                                                               
VKKEYX   DS    0H                                                               
*                                                                               
         B     VKX                                                              
*                                                                               
VKL      DS    0H                  LIST KEY VALIDATION                          
*                                                                               
*        SET NUMBER OF LINES AVAILABLE FOR LIST                                 
*                                                                               
         MVI   NLISTS,(HDLLINLH-HDLLIN1H)/(HDLLIN2H-HDLLIN1H)+1                 
*                                                                               
         LA    R2,HDLMEDH          HEADER OF MEDIA FIELD ON LIST SCREAN         
         OI    VALOPT,VALNAMXQ     DON'T DISPLAY MEDIA NAME IN NEXT FLD         
*                                                                               
         GOTOR VALMED                                                           
*                                                                               
         MVC   HDLMEDN(L'MEDNM),MEDNM                                           
         OI    HDLMEDNH+6,X'80'    DISPLAY NAME                                 
*                                                                               
*        VALIDATE CLIENT                                                        
*                                                                               
         LA    R2,HDLCLTH          HEADER OF CLIENT FIELD                       
         XC    QCLT,QCLT           DEFAULT CLIENT VALUE                         
*                                                                               
         XC    HDLCLTN,HDLCLTN     CLEAR NAME                                   
         OI    HDLCLTNH+6,X'80'    DISPLAY NAME                                 
*                                                                               
         CLI   FLDILEN,0           CHECK IF ENTERED                             
         BE    VKLCLTX             CHECK NEXT FILTER                            
*                                                                               
         OI    VALOPT,VALNAMXQ     DO NOT DISPLAY NAME IN NXT FLD               
*                                                                               
         GOTOR VALCLT              VALIDATE CLT                                 
*                                                                               
         MVC   HDLCLTN(L'HDLCLTN),CLTNM                                         
         OI    HDLCLTNH+6,X'80'    DISPLAY NAME                                 
*                                                                               
VKLCLTX  DS    0H                                                               
*                                                                               
         LA    R2,HDLPUBH          HEADER OF PUB    FIELD                       
         XC    QPUB,QPUB           CLEAR Q VALUE                                
*                                                                               
         XC    HDLPUBN,HDLPUBN     CLEAR NAME                                   
         OI    HDLPUBNH+6,X'80'    DISPLAY NAME                                 
*                                                                               
         CLI   FLDILEN,0           CHECK IF ENTERED                             
         BE    VKLPUBX             CHECK NEXT FILTER                            
*                                                                               
         OI    VALOPT,VALNAMXQ     DO NOT DISPLAY NAME IN NXT FLD               
*                                                                               
         GOTOR VALPUB              VALIDATE PUB                                 
*                                                                               
         MVC   HDLPUBN(L'HDLPUBN),PUBNM                                         
         OI    HDLPUBNH+6,X'80'    DISPLAY NAME                                 
*                                                                               
VKLPUBX  DS    0H                                                               
*                                                                               
*        VALIDATE PERIOD                                                        
*                                                                               
         LA    R2,HDLPERH          HEADER OF PERIOD FIELD                       
         XC    BSTART(L'BSTART+L'BEND),BSTART  CLEAR Q VALUE                    
*                                                                               
         CLI   FLDILEN,0           SKIP IF NOT ENTERED                          
         BE    VKLPERX                                                          
*                                                                               
         GOTOR VALPER              VALIDATE PERIOD                              
*                                                                               
         LA    R3,WORK             PERVAL OUTPUT AREA                           
         USING PERVALD,R3                                                       
         MVC   HDLPER(L'PVALCPER),PVALCPER                                      
         OI    HDLPERH+6,X'80'                                                  
*                                                                               
         DROP  R3                                                               
*                                                                               
VKLPERX  DS    0H                                                               
*                                                                               
VKX      DS    0H                                                               
*                                                                               
         CLI   DDLNKSW,C'Y'       SKIP IF ADBUYER DELETE                        
         BE    VKXX                                                             
         CLI   ACTNUM,ACTABDEL    IF ACTION ABDELETE                            
         BNE   *+8                                                              
         BRAS  RE,DR              DISPLAY REC                                   
*                                                                               
VKXX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
VKMEDER  LHI   RF,PPEFLDNE        MEDIA REQUIRED                                
         B     VKERR                                                            
*                                                                               
VKPERER  LHI   RF,PPEFLDNE        PERIOD REQUIRED                               
         B     VKERR                                                            
*                                                                               
VKCLTER  LHI   RF,PPEFLDNE        CLIENT REQUIRED                               
         B     VKERR                                                            
*                                                                               
VKPUBER  LHI   RF,PPEFLDNE        PUB    REQUIRED                               
         B     VKERR                                                            
*                                                                               
VKIO#ER  LHI   RF,PPEFLDNE        WEB IO   REQUIRED                             
         B     VKERR                                                            
*                                                                               
VKRV#ER  LHI   RF,PPEFLDNE        REVISION INVALID                              
         B     VKERR                                                            
*                                                                               
VKIO#2ER LHI   RF,PPEIO#NF        WEB IO   NOT ON FILE                          
         B     VKERR                                                            
*                                                                               
VKSTWER  LHI   RF,PPEFLDNV        STEWARD OPTION NOT VALID                      
         B     VKERR                                                            
*                                                                               
VKPRDER  LHI   RF,PPEFLDNE        PRODUCT FIELD IS REQUIRED                     
         B     VKERR                                                            
*                                                                               
VKDELER  LHI   RF,PPERECDL        RECORD IS DELETED                             
         B     VKERR                                                            
*                                                                               
VKDEL1ER LHI   RF,PPERECDL        RECORD IS DELETED                             
         B     VKERR                                                            
*                                                                               
VKDUPKER LHI   RF,PPEDUPKY        DUP KEY                                       
         B     VKERR                                                            
*                                                                               
VKCNTCHG LHI   RF,PPECNTCH        CAN'T CHANGE DELETED REC                      
         B     VKERR                                                            
*                                                                               
*                                                                               
VKERR    DS    0H                                                               
*                                                                               
         XC    ERROR,ERROR         CLEAR ERROR FIELD                            
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
         GOTOR ERREXIT                                                          
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPWIO10 - PRINT NEW INSORD CONTROLLER - ABDEL'                  
***********************************************************************         
*                                                                     *         
*        DELETE SEVERAL IOS                                           *         
*                                                                     *         
***********************************************************************         
*                                                                               
ABDEL    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
*                                                                               
ABDELLP  DS    0H                                                               
*                                                                               
         BRAS  RE,VR               HANDLE IO ALREADY  ON SCREEN                 
*                                                                               
ABDELCN  DS    0H                                                               
*                                                                               
         BRAS  RE,VK               PUT NEXT IO ON SCREEN                        
*                                                                               
         CLI   LNKSTSW,C'L'        LOOP IF NOT END OF LIST                      
         BNE   ABDELLP                                                          
*                                                                               
ABDELDN  DS    0H                                                               
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)                                                      
         USING LIOBD,R3               ESTABLISH LINKIO INTERFACE BLOCK          
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOACLO',LIOBD) CLOSE WORKER FILE                 
*                                                                               
ABDELX   DS    0H                                                               
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
         MVC   HDRMED,SPACES                                                    
         MVC   HDRMED(L'QMED),QMED                                              
         MVI   HDRMEDH+5,L'QMED                                                 
*                                                                               
*        SET CLIENT ON SCREEN                                                   
*                                                                               
         MVC   HDRCLT,SPACES                                                    
         MVC   HDRCLT(L'QCLT),QCLT                                              
         MVI   HDRCLTH+5,L'QCLT                                                 
*                                                                               
*        SET INSERTION ORDER # ON SCREEN                                        
*                                                                               
         MVC   HDRIO#,SPACES                                                    
*                                                                               
         LA    R2,HDRIO#H          POINT TO IO# FIELD                           
*                                                                               
         GOTOR DISIO#              DISPLAY IT                                   
*                                                                               
         LA    R2,HDRREV#H         POINT TO REVISION # FLD                      
*                                                                               
         GOTOR DISRV#              DISPLAY IT                                   
*                                                                               
         B     FSCRIO#X                                                         
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,QIO#IOYR       GET IO# YEAR                                 
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  HDRIO#(2),DUB                                                    
*                                                                               
         ICM   RF,7,QIO#IOSQ       GET IO# SEQ NUMBER                           
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  HDRIO#+2(4),DUB                                                  
*                                                                               
         MVI   HDRIO#H+5,6                                                      
*                                                                               
*        SET REVISION # ON SCREEN                                               
*                                                                               
         MVC   HDRREV#,SPACES                                                   
*                                                                               
         SR    RF,RF               CLEAR WORK REGISTER                          
         ICM   RF,1,QREV#          GET REVISION #                               
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  HDRREV#(3),DUB                                                   
*                                                                               
         MVI   HDRREV#H+5,3        SET INPUT LENGTH                             
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
         CHI   RF,L'HDRPUB         ERROR IF TOO LONG                            
         BH    FSCRPUBX                                                         
*                                                                               
         XC    HDRPUB,HDRPUB                                                    
         STC   RF,HDRPUBH+5        SET INPUT LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   HDRPUB(0),WKDTDATA  MOVE TEXT TO SCREEN FIELD                    
*                                                                               
FSCRPUBX DS    0H                                                               
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
         TITLE 'PPWIO10 - PRINT NEW INSORD CONTROLLER - GETINPUT'               
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
         TITLE 'T41E10 - WEB IO HEADER MAINT/LIST - VR'                         
***********************************************************************         
*                                                                     *         
*        VALIDATE WEB IO HEADER FIELDS                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VR       NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
*        IF ACTION ABDELETE                                                     
*                                                                               
         CLI   ACTNUM,ACTABDEL     IF ACTION ABDELETE                           
         BNE   *+12                                                             
         BRAS  RE,RDEL                                                          
         B     VRXX                   ALL DONE                                  
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING                                    
         BNE   VRADDX                                                           
*                                                                               
         LA    R4,KEY              ESTABLISH MASTER KEY                         
         USING WIOKEY,R4                                                        
*                                                                               
         TM    WIODCNTL,WIODDELQ   IF RECORD IS DELETED                         
         BNO   VRADDX                                                           
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO CONTROL BLOCK                
         USING MINBLKD,R7                                                       
*                                                                               
         MVC   MINMKEY,WIOKEY      COPY MASTER KEY                              
*                                  NEED TO REUSE MINIO SET                      
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN  MINIO SET              
         GOTOR VMINIO,WIOPARMS,('MINRSF',MINBLKD)  RESTORE                      
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD)  CLOSE MINIO SET              
*                                                                               
VRADDX   DS    0H                                                               
*                                                                               
*        VALIDATE RUN DATE                                                      
*                                                                               
         LA    R2,HDRDTEH                                                       
*                                                                               
         CLI   FLDILEN,0           REQUIRED FIELD                               
         BE    VRREQERR                                                         
*                                                                               
         XC    WORK,WORK           INIT WORKAREA                                
         LA    R3,WORK             PERVAL OUTPUT AREA                           
         USING PERVALD,R3                                                       
*                                                                               
         MVC   PVALBSTA,BTODAY     PASS TODAY'S DATE                            
*                                                                               
         LA    RF,PVINTOD+PVINSGLO+PVINSGLS  SINGLE DATE                        
*                                                                               
         GOTO1 PERVAL,DMCB,(FLDILEN,FLDDATA),((RF),PERVALD)                     
*                                                                               
         CLI   DMCB+4,0            CHECK FOR ERRORS                             
         BE    VRDTEERR                                                         
*                                                                               
         MVC   QRDATE,PVALBSTA     SAVE WEB IO RUN DATE                         
*                                                                               
         BRAS  RE,CLRFLD           CLEAR PERIOD FIELD                           
*                                                                               
         MVC   FLDDATA(8),PVALCPER  DISPLAY PERIOD                              
*                                                                               
         DROP  R3                                                               
*                                                                               
*        VALIDATE PRODUCT                                                       
*                                                                               
         LA    R2,HDRPRDH          POINT TO PRODUCT FIELD                       
*                                                                               
         CLI   FLDILEN,0           CLIENT IS REQUIRED                           
         BE    VRPRDER                                                          
*                                                                               
         GOTOR VALPRD              VALIDATE PRODUCT                             
*                                                                               
*        VALIDATE ESTIMATE                                                      
*                                                                               
         LA    R2,HDRESTH          POINT TO ESTIMATE FIELD                      
*                                                                               
         MVI   FLDOPT,C'Y'         ESTIMATE IS OPTIONAL                         
         GOTOR VALEST              VALIDATE ESTIMATE                            
*                                                                               
*        VALIDATE STATUS                                                        
*                                                                               
         LA    R2,HDRSTAH           STATUS FIELD                                
*                                                                               
         GOTOR VALSTA                                                           
*                                                                               
*        BUILD MINMKEY                                                          
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO CONTROL BLOCK                
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH WEB IO MASTER KEY                  
         USING WIOKEY,R4                                                        
*                                                                               
         CLI   ACTNUM,ACTADD       CHECK IF ADDING                              
         BE    VR90                                                             
*                                                                               
         MVC   WIOKEY,QIOKEY       IF NOT ADD THEN WE ALREADY HAVE              
*                                  KEY FROM VK                                  
         B     VR95                CONTINUE WITH OPEN MINIO                     
*                                                                               
VR90     DS    0H                                                               
*                                                                               
         OI    QACTCHG1,WIOAADD    INDICATE ELEMENT ADDED                       
*                                                                               
         MVC   WIOKAGY,QAGY        SET AGENCY                                   
         MVC   WIOKMED,QMED        SET MEDIA                                    
         MVI   WIOKRCD,WIOKRCDQ    SET RECORD CODE                              
         MVC   WIOKCLT,QCLT        SET CLIENT                                   
         MVC   WIOKPUB,QPUB        SET PUB                                      
         MVC   WIOKIO#,QIO#        SET IO#                                      
         MVC   WIOKRV#,QREV#       SET REVISION #                               
         MVC   QIOKEY,WIOKEY       SAVE MASTER KEY                              
*                                                                               
VR95     DS    0H                                                               
*                                                                               
*        OPEN MINIO SET                                                         
*                                                                               
         MVI   MINDELSW,C'Y'       OPEN MINIO SET                               
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0            ONLY ERROR TOLERATED                         
         BE    *+8                                                              
         CLI   MINERR,MINESNF      IS NOT FOUND                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   MINERR,MINESNF      IF NOT FOUND                                 
         BNE   *+14                                                             
         CLI   ACTNUM,ACTADD          ACTION MUST BE ADD                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,ELEMENT           ESTABLISH HEADER ELM                        
         XC    ELEMENT,ELEMENT      CLEAR                                       
         USING WIOHDRD,R6                                                       
*                                                                               
         MVI   WIOHKCDE,WIOHKIDQ    HEADER ELEMENT CODE                         
         MVI   WIOHKLEN,WIOHDRLQ    HEADER ELEMENT LENGTH                       
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF ADD                                  
         BE    VR150                                                            
*                                  NOT ADD FUNCTION                             
         TM    MINSTAT,MINDELQ     RECORD CAN'T BE DELETED                      
         BO    VRCNTCHG                                                         
*                                                                               
         GOTOR GETELM,DMCB,WIOHKEY  FIND ELEMENT IN RECORD                      
*                                                                               
         ICM   R6,15,MINELEM        POINT R6 TO FOUND ELEMENT                   
         BNZ   *+6                                                              
         DC    H'0'                MUST FIND HEADER                             
*                                                                               
VR150    DS    0H                                                               
*                                                                               
         USING WIOHDRD,R6                                                       
*                                                                               
         MVC   WIOHDATE,QRDATE      WEB IO DATE - BINARY                        
         MVC   WIOHSTAT,HDRSTA      WEB IO STATUS                               
         MVC   WIOHSTRT,BSTART      PERIOD START DATE - BI                      
         MVC   WIOHEND,BEND         PERIOD END   DATE - BI                      
         MVC   WIOHPRD,QPRD         PRODUCT                                     
         MVC   WIOHEST,BEST         ESTIMATE IN BINARY                          
*                                                                               
         OC    WIOHEST,WIOHEST     IF THERE IS AN ESTIMATE                      
         BZ    *+8                                                              
         OI    WIOHOTYP,WHOTESTQ      SET FLAG                                  
*                                                                               
         MVC   WIOHSTEW,QSTEW       STEWARDSHIP                                 
*                                                                               
VR175    DS    0H                                                               
*                                                                               
         L     RF,WRTELM           ASSUMING RE-WRITING ELEMENT                  
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADD                                       
         BNE   *+8                                                              
         L     RF,ADDELM              USE DIFFERENT ROUTINE                     
*                                                                               
         GOTOR (RF),DMCB,WIOHKEY   WRITE ELEMENT TO RECORD                      
*                                                                               
*        UPDATE ACTIVITY ELEMENT                                                
*                                                                               
         OC    QACTCHG1,QACTCHG1   SKIP IF NO CHANGES                           
         BZ    VRACTX                                                           
*                                                                               
         GOTOR ACTPUT              ADD ACTIVITY ELEMENT                         
*                                                                               
VRACTX   DS    0H                                                               
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
*                                                                               
         CLI   MINERR,0            SHOULD BE NO ERROR                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
         GOTOR PSSVS               BUILD ALL POINTERS                           
                                                                                
*        CALL LINKIO INTERFACE IF NEEDED                                        
*                                                                               
         CLI   DDLNKSW,C'Y'        IF IN A LINK CALL                            
         BNE   VRLNKX                                                           
*                                                                               
         GOTOR LNKPUT,DMCB,(RC)       SEND DATA BACK TO CALLER                  
*                                                                               
VRLNKX   DS    0H                                                               
*                                                                               
*                                                                               
VRX      DS    0H                                                               
         BRAS  RE,DR               RECORD VALIDATED, REDISPLAY IT               
*                                                                               
VRXX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
VRREQERR LHI   RF,PPEFLDNE         FIELD IS REQUIRED                            
         J     VRERR                                                            
*                                                                               
VRPRDER  LHI   RF,PPEFLDNE         PRODUCT FIELD IS REQUIRED                    
         J     VRERR                                                            
*                                                                               
VRCDERR  LHI   RF,PPECDINV         INVALID CD (Y/N)                             
         J     VRERR                                                            
*                                                                               
VRDTEERR LHI   RF,PPEDTENV         INVALID SINGLE DATE                          
         J     VRERR                                                            
*                                                                               
VRMONERR LHI   RF,PPEMONNV         INVALID MONEY                                
         J     VRERR                                                            
*                                                                               
VRMNMERR LHI   RF,PPEMNMNV         MAX IS 9,999,999.99                          
         J     VRERR                                                            
*                                                                               
VRCNTCHG LA    R2,CONACTH                                                       
         LHI   RF,PPECNTCH        CAN'T CHANGE DELETED REC                      
         B     VRERR                                                            
*                                                                               
VRERR    DS    0H                                                               
*                                                                               
         XC    ERROR,ERROR         CLEAR ERROR FIELD                            
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E10 - WEB IO HEADER MAINT/LIST - DR'                         
***********************************************************************         
*                                                                     *         
*        DISPLAY WEB IO HEADER RECORD                                 *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
DR       NTR1  BASE=*,LABEL=*      DISPLAY RECORD                               
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO CONTROL BLOCK                
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH WEB IO MASTER KEY                  
         USING WIOKEY,R4           DISPLAY ACTIVE USING                         
*                                                                               
         MVC   WIOKEY,QIOKEY       SHOULD HAVE KEY OF MASTER KEY                
         MVC   QIO#,WIOKIO#                                                     
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
* OPEN MINIO SET                                                                
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0             SHOULD BE NO ERROR                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,ELEMENT           BUILD ELM                                   
         XC    ELEMENT,ELEMENT      CLEAR                                       
         USING WIOHDRD,R6                                                       
*                                                                               
         MVI   WIOHKEY,WIOHKIDQ     ELEM ID                                     
*                                                                               
         GOTOR GETELM,DMCB,WIOHKEY  GET ELEMENT TO RECORD                       
*                                                                               
         ICM   R6,15,MINELEM        POINT R6 TO ELEMENT                         
         BNZ   *+6                  CHECK IF ITS THERE                          
         DC    H'0'                                                             
*                                                                               
*        DISPLAY MEDIA                                                          
*                                                                               
         LA    R2,HDRMEDH                                                       
         MVC   QMED,WIOKMED                                                     
         GOTOR DISMED              DISPLAY MEDIA                                
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
*        DISPLAY CLIENT                                                         
*                                                                               
         LA    R2,HDRCLTH                                                       
         MVC   QCLT,WIOKCLT                                                     
         GOTOR DISCLT              DISPLAY CLIENT                               
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
*        DISPLAY IO #                                                           
*                                                                               
         LA    R2,HDRIO#H                                                       
         MVC   QIO#,WIOKIO#                                                     
         GOTOR DISIO#                                                           
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
*        DISPLAY RUN DATE                                                       
*                                                                               
         LA    R2,HDRDTEH                                                       
         BRAS  RE,CLRFLD           INIT FIELD                                   
         XC    HDRDTE,HDRDTE       WEB IO DATE                                  
         GOTO1 DATCON,DMCB,(3,WIOHDATE),(11,HDRDTE)                             
         OI    HDRDTEH+6,X'80'                                                  
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
*        DISPLAY STATUS                                                         
*                                                                               
         LA    R2,HDRSTAH                                                       
         MVC   QSTAT,WIOHSTAT                                                   
         GOTOR DISSTA                                                           
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
*        DISPLAY PERIOD                                                         
*                                                                               
         LA    R2,HDRPERH                                                       
         MVC   QPER,WIOHSTRT                                                    
         GOTOR DISPER,DMCB,QPER    DISPLAY PERIOD                               
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
*        DISPLAY EXPANDED IO#                                                   
*                                                                               
         LA    R2,HDRIO#NH         EXPANDED IO#                                 
*                                                                               
         BRAS  RE,CLRFLD           CLEAR THE FIELD                              
*                                                                               
         GOTOR FMTIO#              FORMAT THE IO#                               
*                                                                               
         MVC   FLDDATA(L'QIO#EXP),QIO#EXP DISPLAY IO#                           
*                                                                               
*        DISPLAY PRODUCT                                                        
*                                                                               
         LA    R2,HDRPRDH                                                       
         MVC   QPRD,WIOHPRD                                                     
         GOTOR DISPRD              DISPLAY PRODUCT                              
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
*        DISPLAY STEWARD OPTION                                                 
*                                                                               
         LA    R2,HDRSTEWH                                                      
         MVC   QSTEW,WIOHSTEW                                                   
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OLD DATA                               
*                                                                               
         MVC   FLDDATA(L'QSTEW),QSTEW                                           
*                                                                               
*        DISPLAY ESTIMATE                                                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WIOHEST        GET ESTIMATE                                 
         BZ    DRESTX              SKIP IF NO ESTIMATE NUMBER                   
*                                                                               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  QEST,DUB            CONVERTED TO CHARACTER                       
*                                                                               
         LA    R2,HDRESTH                                                       
*                                                                               
         GOTOR DISEST              DISPLAY ESTIMATE                             
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRESTX   DS    0H                                                               
*                                                                               
*        DISPLAY STATUS                                                         
*                                                                               
         LA    R2,HDRSTAH           STATUS FIELD                                
*                                                                               
         GOTOR DISSTA                                                           
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
*                                                                               
         CLI   MINERR,0             SHOULD BE NO ERROR                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
DRERR    DS    0H                                                               
*                                                                               
         XC    ERROR,ERROR         CLEAR ERROR FIELD                            
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E10 - WEB IO HEADER MAINT/LIST - DK'                         
***********************************************************************         
*                                                                     *         
*        DISPLAY KEY                                                  *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
DK       NTR1  BASE=*,LABEL=*      DISPLAY KEY                                  
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO CONTROL BLOCK                
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY                                                       
         USING WIOKEY,R4           ESTABLISH MASTER KEY                         
*                                                                               
         CLI   ACTNUM,ACTABDEL     OR ACTION ABDELETE                           
         BNE   DK00                                                             
*                                                                               
         MVC   WIOKAGY,QAGY           SET AGENCY CODE                           
         MVC   WIOKMED,QMED           SET MEDIA                                 
         MVI   WIOKRCD,WIOKRCDQ       SET RECORD CODE                           
         MVC   WIOKCLT,QCLT           SET CLIENT                                
         MVC   WIOKPUB,QPUB           SET PUB                                   
         MVC   WIOKIO#,QIO#           SET WEB IO SERIAL NUMBER                  
         MVC   WIOKRV#,QREV#          SET WEB IO REVISION NUMBER                
*                                                                               
         B     DK05                                                             
*                                                                               
DK00     DS    0H                                                               
*                                                                               
         MVC   MINMKEY(L'WIOKEY),KEY  MOVE KEY TO MASTER KEY                    
         MVC   QIOKEY(L'WIOKEY),KEY   SAVE KEY  IN Q AREA                       
*                                                                               
DK05     DS    0H                                                               
*                                                                               
* OPEN MINIO SET                                                                
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        SAVE KEY FIELDS                                                        
*                                                                               
         MVC   QMED,WIOKMED                                                     
         MVC   QCLT,WIOKCLT                                                     
         MVC   QPUB,WIOKPUB                                                     
         MVC   QIO#,WIOKIO#                                                     
         MVC   QREV#,WIOKRV#                                                    
*                                                                               
         LA    R6,ELEMENT           BUILD ELM                                   
         XC    ELEMENT,ELEMENT      CLEAR                                       
         USING WIOHDRD,R6                                                       
*                                                                               
         MVI   WIOHKEY,WIOHKIDQ     ELEM ID                                     
*                                                                               
         GOTOR GETELM,DMCB,WIOHKEY  GET ELEMENT TO RECORD                       
*                                                                               
         ICM   R3,15,MINELEM        POINT R3 TO ELEMENT                         
         BNZ   *+6                  CHECK IF ITS THERE                          
         DC    H'0'                                                             
*                                                                               
*        DISPLAY MEDIA                                                          
*                                                                               
         LA    R2,HDRMEDH          POINT TO MEDIA FIELD HEADER                  
*                                                                               
         BRAS  RE,SETLEN                                                        
*                                                                               
         GOTOR DISMED              DISPLAY MED AND MEDIA NAME                   
*                                                                               
*        DISPLAY CLIENT                                                         
*                                                                               
         LA    R2,HDRCLTH          POINT TO CLIENT FIELD HEADER                 
*                                                                               
         GOTOR DISCLT              DISPLAY CLT AND CLIENT NAME                  
*                                                                               
         BRAS  RE,SETLEN                                                        
*                                                                               
*        DISPLAY PUB NUMBER                                                     
*                                                                               
         LA    R2,HDRPUBH          POINT TO PUB FIELD HEADER                    
*                                                                               
         GOTOR DISPUB              DISPLAY PUB AND PUB NAME                     
*                                                                               
         BRAS  RE,SETLEN                                                        
*                                                                               
         LA    R2,HDRIO#H          POINT TO IO# FIELD HEADER                    
*                                                                               
         GOTOR DISIO#              DISPLAY IO#                                  
                                                                                
         BRAS  RE,SETLEN                                                        
*                                                                               
         LA    R2,HDRREV#H         POINT TO REV# FIELD HEADER                   
*                                                                               
         GOTOR DISRV#              DISPLAY REV#                                 
*                                                                               
         BRAS  RE,SETLEN                                                        
*                                                                               
DKX      DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E10 - WEB IO HEADER MAINT/LIST - LR'                         
***********************************************************************         
*                                                                     *         
*        LIST RECORDS                                                 *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
LR       NTR1  BASE=*,LABEL=*      LIST RECORDS                                 
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         LA    R4,KEY                                                           
         OC    KEY,KEY             IF NOT FIRST TIME TO LIST                    
         BNZ   LRKEYX                 CONTINUE LISTING                          
*                                                                               
*        BUILD STARTING KEY                                                     
*                                                                               
         XC    KEY,KEY                                                          
         USING WIOKEY,R4           ESTABLISH RECORD KEY                         
*                                  SET UP INITIAL KEY                           
         MVC   WIOKAGY,QAGY        AGENCY                                       
         MVC   WIOKMED,QMED        MEDIA                                        
         MVI   WIOKRCD,WIOKRCDQ    RECORD ID                                    
*                                                                               
         MVC   QLCLT,QCLT          COPY FILTERS                                 
         MVC   QLPUB,QPUB                                                       
         MVC   QLPRD,QPRD                                                       
*                                                                               
         CLC   QLCLT,=8X'FF'        IF FILTERING ON CLIENT                      
         BE    *+10                                                             
         MVC   WIOKCLT,QLCLT           SET STARTING CLIENT                      
*                                                                               
         CLC   QLPUB,=8X'FF'        IF FILTERING ON PUB                         
         BE    *+10                                                             
         MVC   WIOKPUB,QLPUB        STARTING PUB                                
*                                                                               
LRKEYX   DS    0H                                                               
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTOR HIGH                FIRST RECORD                                 
*                                                                               
LRLOOP   DS    0H                                                               
*                                                                               
         CLC   WIOKEY(WIOKCLT-WIOKEY),KEYSAVE AGY/MED/RECORD CODE?              
         BNE   LRDONE                                                           
*                                                                               
         CLC   QLCLT,=8X'FF'        SKIP IF NOT FILTERING ON CLIENT             
         BE    LRFLCLTX                                                         
         OC    QLCLT,QLCLT           SKIP IF NOT FILTERING ON CLIENT            
         BZ    LRFLCLTX                                                         
*                                                                               
         CLC   WIOKCLT,QLCLT        DONE ON CHANGE IN CLIENT                    
         BNE   LRDONE                                                           
*                                                                               
LRFLCLTX DS    0H                                                               
*                                                                               
         CLC   QLPUB,=8X'FF'        SKIP IF NOT FILTERING ON PUB                
         BE    LRFLPUBX                                                         
         OC    QLPUB,QLPUB           SKIP IF NOT FILTERING ON PUB               
         BZ    LRFLPUBX                                                         
*                                                                               
         CLC   WIOKPUB,QLPUB        SKIP IF WRONG PUB                           
         BNE   LRCONT                                                           
*                                                                               
LRFLPUBX DS    0H                                                               
*                                                                               
         CLC   WIOKELMK,=8X'FF'    SKIP IF NOT MASTER KEY                       
         BNE   LRCONT                                                           
*                                                                               
         TM    WIODCNTL,WIODDELQ   SKIP IF DELETED                              
         BO    LRCONT                                                           
*                                                                               
         MVC   DMDSKADD,WIODDISK   SAVE DISK ADDR FOR LIST                      
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO CONTROL BLOCK                
         USING MINBLKD,R7                                                       
*                                                                               
         MVC   MINMKEY(L'WIOKEY),WIOKEY                                         
*                                                                               
*        OPEN MINIO SET                                                         
*                                                                               
         MVI   MINDELSW,C'Y'       READ FOR DELETES                             
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                NO ERRORS TOLERATED                          
*                                                                               
         LA    R4,MINMKEY          POINT TO IO MASTER KEY                       
*                                                                               
         MVC   QMED,WIOKMED        SAVE MEDIA                                   
         MVC   QCLT,WIOKCLT        CLIENT                                       
         MVC   QPUB,WIOKPUB        PUB                                          
         MVC   QIO#,WIOKIO#        IO#                                          
         MVC   QREV#,WIOKRV#       REVISION #                                   
*                                                                               
*        READ HEADER ELEMENT                                                    
*                                                                               
         LA    R6,ELEMENT           BUILD ELM                                   
         XC    ELEMENT,ELEMENT      CLEAR                                       
         USING WIOHDRD,R6                                                       
*                                                                               
         MVI   WIOHKEY,WIOHKIDQ     ELEM ID                                     
*                                                                               
         GOTOR GETELM,DMCB,WIOHKEY  GET ELEMENT TO RECORD                       
*                                                                               
         ICM   R6,15,MINELEM        POINT R6 TO ELEMENT                         
         BNZ   *+6                  CHECK IF ITS THERE                          
         DC    H'0'                                                             
*                                                                               
         LA    R5,LISTAR                                                        
         XC    LISTAR(DLISTEQU),LISTAR                                          
         USING DLISTD,R5           ESTABLISH LIST LINE                          
*                                                                               
         MVI   DLISTSTT,C' '                                                    
*                                                                               
         TM    MINSTAT,MINDELQ     CHECK IF DELETED                             
         BNO   *+8                                                              
         MVI   DLISTSTT,C'D'                                                    
*                                                                               
         MVC   DLISTCLT,WIOKCLT     CLIENT                                      
*                                                                               
         MVC   LSPUB,SPACES        INIT PUB FIELD                               
*                                                                               
         GOTO1 VPUBEDIT,DMCB,(0,WIOKPUB),(C'S',LSPUB)                           
*                                                                               
         CLI   0(R1),X'FF'         CHECK FOR ERRORS                             
         BNE   *+6                                                              
         DC    H'0'                SHOULD HAVE VALID PUB CODE                   
*                                                                               
         MVC   DLISTPUB,LSPUB                                                   
*                                                                               
         MVC   DLISTPBN,SPACES     INIT PUB NAME                                
*                                                                               
         GOTOR DISPBNM,DMCB,WIOKPUB,DLISTPBN                                    
*                                                                               
*        DISPLAY IO#                                                            
*                                                                               
         GOTOR FMTIO#              FORMAT  IO #                                 
*                                                                               
         MVC   DLISTIO#,QIO#EXP    ADD IO# TO LIST                              
*                                                                               
         MVC   DLISTPRD,WIOHPRD     PRODUCT                                     
*                                                                               
***                                                                             
***      INPUT INDICATORS                                                       
***            X'80' - RETURN DATE IN P2(1)                                     
***            X'10' - START AND END DATES GIVEN                                
***            X'03' - DATES IN BINARY FORMAT                                   
***      OUTPUT INDICATORS                                                      
***            17    - MMMDD/YY-MMMDD/YY                                        
***                                                                             
*                                                                               
         GOTO1 DATCON,WIOPARMS,(X'93',WIOHSTRT),(17,DLISTPER)                   
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD)  CLOSE MINIO SET              
*                                                                               
         GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
         MVC   KEY,MINMKEY         RESET FILE POINTERS                          
*                                                                               
         GOTOR HIGH                                                             
*                                                                               
LRCONT   DS    0H                                                               
*                                                                               
         LA    R4,KEY              RE-POINT TO SERACH KEY                       
*                                                                               
         GOTOR SEQ                 NEXT RECORD                                  
*                                                                               
         B     LRLOOP                                                           
*                                                                               
LRDONE   DS    0H                                                               
*                                                                               
LRX      DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E10 - WEB IO HEADER MAINT/LIST - PR'                         
***********************************************************************         
*                                                                     *         
*        PRINT RECORDS                                                *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
PR       NTR1  BASE=*,LABEL=*      PRINT RECORDS                                
*                                                                               
PRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E10 - WEB IO HEADER MAINT/LIST - RS'                         
***********************************************************************         
*                                                                     *         
*        RESTORE RECORD                                               *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
RS       NTR1  BASE=*,LABEL=*      RESTORE RECORDS                              
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO CONTROL BLOCK                
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH WEB IO MASTER KEY                  
         USING WIOKEY,R4                                                        
*                                                                               
         MVC   WIOKEY,QIOKEY       MOVE MASTER KEY                              
*                                                                               
*        OPEN  MINIO                                                            
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0                                                         
         BE    *+6                 NO ERRORS TOLERATED                          
         DC    H'0'                                                             
*                                                                               
         TM    MINSTAT,MINDELQ     CHECK IF DELETED                             
         BNO   RSMSTBDL                                                         
*                                                                               
         GOTO1 VMINIO,DMCB,('MINRSF',(R7))                                      
*                                                                               
         CLI   MINERR,0                                                         
         BE    RS09                                                             
*                                                                               
         CLI   MINERR,MINENDEL     RECORD SET NOT DELETED, 2 USERS              
         BE    RSMSTBDL                                                         
         DC    H'0'                                                             
*                                                                               
RS09     DS    0H                                                               
*                                                                               
         GOTOR PSSVS               IT WILL RESTORE PASSIVE POINTER              
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
*                                                                               
*        RE-OPEN AND CREATE ACTIVITY ELEM                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH WEB IO MASTER KEY                  
         USING WIOKEY,R4                                                        
*                                                                               
         MVC   WIOKEY,QIOKEY       MOVE MASTER KEY                              
*                                                                               
*        OPEN  MINIO                                                            
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
*        CREATE RESTORE ACTIVITY ELEMENT FOR WEB IO                             
*                                                                               
*                                                                               
         OI    QACTCHG1,WIOARST    INDICATE RESTORE TOOK PLACE                  
*                                                                               
         GOTOR ACTPUT              ADD ACTIVITY ELEMENT                         
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
*                                                                               
RSX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
RSMSTBDL LA    R2,CONACTH                                                       
         LHI   RF,PPEMSTDL         CAN'T RESTORE A LIVE RECORD                  
         J     RSRECERR                                                         
*                                                                               
RSRECERR DS    0H                                                               
*                                                                               
         XC    ERROR,ERROR         CLEAR ERROR FIELD                            
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E10 - WEB IO HEADER MAINT/LIST - GETEL'                      
***********************************************************************         
*                                                                     *         
*        GETEL MACRO                                                  *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E10 - WEB IO HEADER MAINT/LIST - DISPBNM'                    
***********************************************************************         
*                                                                     *         
*        DISPLAY PUB NAME                                             *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
DISPBNM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         L     R3,0(R1)            LOAD ADDRESS OF PUB CODE                     
         L     R5,4(R1)            LOAD ADDRESS OF OUTPUT AREA                  
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBRECD,R4          ESTABLISH PUB RECORD KEY                     
*                                                                               
         MVC   PUBKMED,QMED                                                     
         MVC   PUBKPUB(6),0(R3)    MOVE PUB/ZONE/EDTN                           
         MVC   PUBKAGY,AGENCY                                                   
         MVI   PUBKCOD,X'81'                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PUBDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   DISPBNMX            PUB NOT FOUND                                
*                                                                               
         L     R6,AIO2             READ IN PUB RECORD                           
         ST    R6,AIO                                                           
         MVC   FILENAME,=CL8'PUBFILE'                                           
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                  FIND NAME ELEMENT                            
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PUBNAMEL,R6                                                      
         MVC   0(L'DLISTPBN,R5),PUBNAME                                         
*                                                                               
DISPBNMX DS    0H                                                               
*                                                                               
*        RESTORE FILE POINTERS                                                  
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY(L'MYKEY),MYKEY  RESTORE KEY                                  
         GOTO1 HIGH                                                             
         LA    R4,KEY                                                           
         USING WIOKEY,R4           ESTABLISH WIO RECORD KEY                     
         MVC   DMDSKADD,WIODDISK   SAVE CURRENT DISK ADDRESS                    
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E10 - WEB IO HEADER MAINT/LIST - RDEL'                       
***********************************************************************         
*                                                                     *         
*        DELETE MINIO SET                                             *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
RDEL     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         GOTOR TSTLOK              MAKE SURE CLIENT IS NOT LOCKED               
         BNE   RDELLKER            CLIENT LOCKED                                
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO CONTROL BLOCK                
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH WEB IO MASTER KEY                  
         USING WIOKEY,R4                                                        
*                                                                               
         MVC   WIOKEY,QIOKEY       MOVE MASTER KEY                              
*                                                                               
*        OPEN  MINIO                                                            
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0                                                         
         BE    *+6                 NO ERRORS TOLERATED                          
         DC    H'0'                                                             
*                                                                               
         TM    MINSTAT,MINDELQ                                                  
         BO    DRCNTDEL                                                         
*                                                                               
         GOTOR DELPSSV             DELETE PASSIVE POINTERS                      
*                                  DO BEFORE CLEARING ALL ELEMENTS              
*                                                                               
*        CLEAR ALL ELEMENTS FROM THE RECORD                                     
*                                                                               
         LA    R2,BSER#TB          POINT TO BUY SERIAL NUMBER TABLE             
         XC    0(5,R2),0(R2)       INIT FIRST ELEMENT                           
*                                                                               
         XC    MINEKEY,MINEKEY     INIT ELEMENT KEY                             
*                                                                               
         MVI   MINEKEY,01          READ FIRST ELEMENT IN MINIO SET              
*                                                                               
RDELLOOP DS    0H                                                               
*                                  READS NEXT ELEMENT                           
         GOTOR VMINIO,WIOPARMS,('MINHI',MINBLKD)                                
*                                                                               
         CLI   MINERR,0                                                         
         BNE   RDELDONE            END OF MINIO SET                             
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
         USING WIOBUYD,R6          ESTABLISH BUY ELEMENT                        
*                                                                               
         CLI   WIOBKCDE,WIOBKIDQ   IF BUY SERIAL NUMBER ELEMENT                 
         BNE   RDELBUYX                                                         
*                                                                               
         MVC   0(5,R2),WIOBSER#    SAVE SERIAL NUMBER                           
*                                                                               
         LA    R2,5(R2)               BUMP TO NEXT SERIAL# SAVEAREA             
         XC    0(5,R2),0(R2)          INITIALIZE                                
*                                                                               
RDELBUYX DS    0H                                                               
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINDEL',MINBLKD)  DELETE ELEMENT               
*                                                                               
         CLI   MINERR,0                                                         
         BE    *+6                 NO ERRORS TOLERATED                          
         DC    H'0'                                                             
*                                                                               
RDELCONT DS    0H                                                               
*                                                                               
         B     RDELLOOP                                                         
*                                                                               
RDELDONE DS    0H                                                               
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD) CLOSE   MINIO SET             
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD) RE-OPEN MINIO SET             
*                                                                               
*        DELETE MINIO SET                                                       
*                                                                               
         GOTO1 VMINIO,DMCB,('MINDLF',(R7))                                      
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        DELETE WEBIO ELEMENTS FROM BUYS                                        
*                                                                               
         LA    R3,BSER#TB          POINT TO START OF SERIAL # TABLE             
         CR    R2,R3               SKIP IF TABLE IS EMPTY                       
         BE    RDELBDLX                                                         
*                                                                               
         LA    R4,KEY              ESTABLISH SERIAL # PASSIVE                   
         USING PSERKEY,R4                                                       
         XC    KEY,KEY                                                          
*                                                                               
         MVC   PSERKAGY,AGENCY     SET AGENCY                                   
         MVC   PSERKMED,QMED       MEDIA                                        
         MVI   PSERKRCD,PSERKIDQ   SET RECORD ID                                
         MVC   PSERKCLT,QCLT       CLIENT                                       
*                                                                               
RDELBDLP DS    0H                                                               
*                                                                               
         OC    0(5,R3),0(R3)       DONE IF END OF TABLE                         
         BZ    RDELBDDN                                                         
*                                                                               
         ZAP   DUB,=P'1000000000'                                               
         SP    DUB,0(5,R3)         9'S COMPLEMENT OF SERIAL #                   
*                                                                               
         MVC   PSERKNUM,DUB+3      SET SERIAL NUMBER                            
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTOR HIGH                READ PASSIVE                                 
*                                                                               
         CLC   PSERKEY,KEYSAVE     SKIP IF NOT FOUND                            
         BNE   RDELBDCN                                                         
*                                                                               
         MVI   RDUPDATE,C'Y'       SET FOR UPDATE                               
*                                                                               
         GOTOR GETREC              READ IN RECORD                               
*                                                                               
         MVI   ELCODE,PWIOELCQ     SET TO FIND WEBIO ELEMENT                    
         L     R6,AIO              POINT TO FOUND RECORD                        
*                                                                               
         BRAS  RE,GETEL            FIND SERIAL NUMBER ELEMENT                   
*                                                                               
RDELSRLP DS    0H                                                               
*                                                                               
         BNE   RDELSRDN            END OF RECORD                                
*                                                                               
         USING PWIOELEM,R6         ESTABLISH WEBIO ELEMENT                      
*                                                                               
         CLC   PWIONUMB,QIO#       MATCH ON IO NUMBER                           
         BE    RDELSRFD                                                         
*                                                                               
RDELSRCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT ELEMENT                            
*                                                                               
         B     RDELSRLP                                                         
*                                                                               
RDELSRFD DS    0H                                                               
*                                                                               
         XC    SVEIOADC,SVEIOADC                                                
         CLI   PWIOMODC,C'N'       NEW ON ORDER?                                
         BNE   *+16                                                             
         MVC   SVEIOSTA,PWIOSTAT                                                
         MVC   SVEIOADC,PWIOADCD                                                
*                                                                               
         GOTOR VRECUP,DMCB,(X'01',AIO),PWIOELEM,0    DELETE ELM                 
*                                                                               
*                                                                               
*        FIND FIRST FOLLOWING CHANGE ELEMENT                                    
*              REMOVE IO SENT INDICATOR                                         
*                                                                               
         USING PCHGELEM,R6         ESTABLISH CHANGE ELEMENT                     
*                                                                               
         SR    RF,RF                                                            
*                                                                               
RDELCHGL DS    0H                                                               
*                                                                               
         CLI   PCHGELEM,0          DONE IF END OF RECORD                        
         BE    RDELCHGD                                                         
*                                                                               
         CLI   PCHGELEM,PWIOELCQ   DONE IF NEXT IO ELM FOUND                    
         BE    RDELCHGD                                                         
*                                                                               
         CLI   PCHGELEM,PCHGELQ    SKIP IF NOT A CHGELEM                        
         BNE   RDELCHGC                                                         
*                                                                               
         TM    PCHGIND4,X'02'      OKAY IF AN IO SENT CHGELEM                   
         BO    RDELCHGF                                                         
*                                                                               
RDELCHGC DS    0H                                                               
*                                                                               
         IC    RF,PCHGLEN          GET ELEMENT LENGTH                           
         AR    R6,RF               BUMP TO NEXT ELEMENT                         
         B     RDELCHGL                                                         
*                                                                               
RDELCHGF DS    0H                  CORRECT CHGELEM FOUND                        
*                                                                               
         NI    PCHGIND4,X'FF'-X'02' TURN OFF IO SENT INDICATOR                  
*                                                                               
*        DELETE ELEMENT IF NO OTHER CHANGES                                     
*                                                                               
         CLI   PCHGIND1,0          TEST FOR ANY OTHER CHANGES INDICATED         
         BNE   RDELCHGD               YES                                       
         CLI   PCHGIND2,0                                                       
         BNE   RDELCHGD                                                         
         CLI   PCHGIND3,0                                                       
         BNE   RDELCHGD                                                         
         CLI   PCHGIND4,0                                                       
         BNE   RDELCHGD                                                         
*                                                                               
*        CHECK IF FIFTH INDICATOR PRESENT                                       
*                                                                               
         CLI   PCHGLEN,PCHGNEWS    IF NEW SHORT LENGTH                          
         BNE   *+12                                                             
         LA    R1,PCHGELEM+PCHGNEWS-1 POINT TO 5TH IND                          
         B     RDELCHG1                                                         
*                                                                               
         CLI   PCHGLEN,PCHGNEWL    IF NEW LONG  LENGTH                          
         BNE   RDELCHG2                                                         
         LA    R1,PCHGELEM+PCHGNEWL-1 POINT TO 5TH IND                          
*                                                                               
RDELCHG1 DS    0H                                                               
*                                                                               
         CLI   0(R1),0             CHECK IF ANY CHANGES                         
         BNE   RDELCHGD               YES                                       
*                                                                               
*        NO OTHER CHANGES INDICATED IN ELEMENT                                  
*              DELETE ELEMENT                                                   
*                                                                               
RDELCHG2 DS    0H                                                               
*                                                                               
         GOTOR VRECUP,DMCB,(X'01',AIO),PCHGELEM,0    DELETE ELM                 
*                                                                               
RDELCHGD DS    0H                                                               
*                                                                               
RDELSRDN DS    0H                                                               
*                                                                               
         GOTOR PUTREC              RE-WRITE BUY                                 
*                                                                               
RDELBDCN DS    0H                                                               
*                                                                               
         MVC   WORK,KEYSAVE        SAVE KEY                                     
         OC    SVEIOADC,SVEIOADC                                                
         BZ    RDELSRMP                                                         
         L     RF,AIO              POINT TO BUY RECORD                          
         USING PBUYKEY,RF                                                       
         CLI   PBUYKRCD,X'20'                                                   
         BE    *+6                                                              
         DC    H'0'                NOT BUY RECORD                               
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(2,FULL)                                
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING PMTPKEY,RE                                                       
         L     RF,AIO              POINT TO BUY RECORD                          
         MVC   PMTPKAGY,PBUYKAGY                                                
         MVC   PMTPKMED,PBUYKMED                                                
         MVI   PMTPKRCD,PMTPKRCQ                                                
         MVC   PMTPKCLT,PBUYKCLT                                                
         MVC   PMTPKPRD,PBUYKPRD                                                
         MVC   PMTPKADC,SVEIOADC                                                
         MVC   PMTPKPUB,PBUYKPUB                                                
         TM    SVEIOSTA,PWIOSMAQ   MAT= REPEAT ACROSS ZONE/EDITION?             
         BNZ   *+16                                                             
         MVC   PMTPKZON,PBUYKZON                                                
         MVC   PMTPKEDT,PBUYKEDT                                                
         MVC   PMTPKDAT,FULL                                                    
         MVC   PMTPKLIN,PBUYKLIN                                                
         GOTOR HIGH                                                             
         CLC   KEY(L'PMTPKEY),KEYSAVE                                           
         BNE   RDELSRMP                                                         
         LA    RE,KEY                                                           
         OI    PMTPCNT1,PMTPC1DQ   FLAG FOR DELETION                            
         GOTOR WRITE                                                            
         DROP  RE,RF                                                            
*                                                                               
RDELSRMP DS    0H                  END OF MAT= REPEAT PASSIVE PTR DEL           
         MVC   KEYSAVE,WORK        RESTORE BASE KEY                             
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE BASE KEY                             
         LA    R3,5(R3)            BUMP TO NEXT TABLE ENTRY                     
         B     RDELBDLP                                                         
*                                                                               
RDELBDDN DS    0H                                                               
*                                                                               
RDELBDLX DS    0H                                                               
*                                                                               
         CLI   DDLNKSW,C'Y'        SKIP IF NOT A LINK CALL                      
         BNE   RDELLNKX                                                         
*                                                                               
*        SEND RECORD CODE                                                       
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)                                                      
         USING LIOBD,R3               ESTABLISH LINKIO INTERFACE BLOCK          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,LNKREPCD       GET REPLY CODE                               
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',(R0))                  
*                                                                               
*        SEND IO KEY                                                            
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#IOLKEY),    X        
               ('LD_CHARQ',LNKWIOKY),(L'LNKWIOKY,0)                             
*                                                                               
RDELLNKX DS    0H                                                               
*                                                                               
RDELX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
RDELLKER LHI   RF,PPELOCKD                                                      
         J     VDRECERR                                                         
*                                                                               
DRCNTDEL LHI   RF,PPERECDL                                                      
         J     VDRECERR                                                         
*                                                                               
VDRECERR DS    0H                                                               
*                                                                               
         XC    ERROR,ERROR         CLEAR ERROR FIELD                            
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
*                                                                               
         CLI   DDLNKSW,C'Y'        IF IN DDLINK CALL                            
         BNE   VDRECERX                                                         
*                                                                               
*        BUILD ERROR MESSAGE FOR RETURN TO ADBUYER                              
*                                                                               
*        DISPLAY ERROR MESSAGE IN SCREEN MESSAGE LINE                           
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
         OI    GENSTAT2,USMYERSY   USE MY ERROR MESSAGES                        
*                                                                               
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
*                                                                               
         CLI   ERROR,0             IF OLD STYLE MESSAGE NUMBER                  
         BE    *+10                                                             
         MVC   PERROR+1(1),ERROR      PUT IN NEW STYLE                          
*                                                                               
         MVC   GTINDX,PINDEX       MESSAGE INDEX                                
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         MVC   GTMSGNO,PERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,PMSGTYPE     MESSAGE TYPE                                 
         MVC   GTLTXT,PTXTLEN      LENGTH OF OPTIONAL TEXT                      
         MVC   GTATXT,PTXTADR      A(OPTIONAL TEXT)                             
*                                                                               
         MVC   GTMSYS,GETMSYS      DEFAULT TO HOST MESSAGES                     
*                                                                               
         OI    6(R2),X'40'         POSITION CURSOR TO THIS FIELD                
         OI    CONHEADH+6,X'80'    ALWAYS TRANSMIT HEADER                       
*                                                                               
*        DISPLAY MESSAGE ON SCREEN                                              
*                                                                               
         LA    RE,CONHEADH         POINT TO ERROR MESSAGE AREA                  
         STCM  RE,7,GTAOUT         PASS TO GETTXT                               
         MVI   GTMAXL,L'CONHEAD    SET MAX MESSAGE LENGTH                       
*                                                                               
         GOTO1 GETTXT,GETTXTCB     DISPLAY ERROR MESSAGE                        
*                                                                               
*        SEND ERROR REPLY CODE                                                  
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)                                                      
         USING LIOBD,R3            ESTABLISH LINKIO INTERFACE BLOCK             
*                                                                               
         LHI   R0,E#IODLER         SET REPLY CODE                               
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',(R0))                  
*                                                                               
*        SEND IO KEY                                                            
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#IOLKEY),    *        
               ('LD_CHARQ',LNKWIOKY),(L'LNKWIOKY,0)                             
*                                                                               
         LHI   R0,D#MEDCOD         INDICATE MEDIA CODE IN ERROR                 
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERRNUM),    *        
               ('LD_UBINQ',(R0)),(2,0)                                          
*                                                                               
*        RETURN ERROR MESSAGE                                                   
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERRDSC),    *        
               ('LD_CHARQ',CONHEAD),(L'CONHEAD,0)                               
*                                                                               
         XIT1                      GO PROCESS NEXT DELETE REQUEST               
*                                                                               
VDRECERX DS    0H                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPWIO00 - PRINT NEW WEB IO CONTROLLER - CLRFLD'                 
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
         AHI   RF,-8                  DECREMENT BY EXTENDED HDR LENGTH          
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
***********************************************************************         
         TITLE 'PPWIO00 - PRINT NEW WEB IO CONTROLLER - SETLEN'                 
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
         AHI   RF,-8                  DECREMENT BY EXTENDED HDR LENGTH          
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
         TITLE 'PPWIO00 - PRINT NEW WEB IO CONTROLLER - BUMP'                   
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
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
          TITLE 'T41E10 - FAX MAINT/LIST - WKRDATD'                             
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
         TITLE 'PPWIO00 - PRINT WEB IO CONTROLLER - STATUSTD'                   
***********************************************************************         
*                                                                     *         
*        DSECT FOR LIST LINE                                          *         
*                                                                     *         
***********************************************************************         
*                                                                               
DLISTD   DSECT                                                                  
DLISTSTT DS    CL1                 DELETED OR NOT                               
         DS    CL1                                                              
DLISTCLT DS    CL3                 CLIENT                                       
         DS    CL1                                                              
DLISTPUB DS    CL14                PUB                                          
         DS    CL1                                                              
DLISTPBN DS    CL14                PUB NAME                                     
         DS    CL1                                                              
DLISTIO# DS    CL19                IO #                                         
         DS    CL1                                                              
DLISTPRD DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
DLISTPER DS    CL17                PERIOD                                       
DLISTEQU EQU   *-DLISTD                                                         
*                                                                               
         PRINT OFF                                                              
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
         PRINT ON                                                               
*                                                                               
       ++INCLUDE PPWIOFFD                                                       
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPWIOFED                                                       
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPWIOFDD          WEB IO HEADER MAINT SCREEN                   
         EJECT                                                                  
*                                                                               
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPWIOWRKD                                                      
         ORG   SYSSPARE            WORKING AREA                                 
*                                                                               
RELO10   DS    F                   RELOACTION FACTOR                            
*                                                                               
LSSVKEY  DS    CL(L'KEY)           SAVED KEY FROM LIST ROUTINE                  
MYKEY    DS    CL25                                                             
XKEY1    DS    CL25                                                             
XKEY2    DS    CL25                                                             
XKEY3    DS    CL25                                                             
LSPUB    DS    CL28                AREA FOR PUB NUMBER EBCDIC                   
LQIO#    DS    CL(L'QIO#)                                                       
QMONEY   DS    PL6                                                              
CHGSWTCH DS    CL1                 C'Y' - A KEY FIELD HAS BEEN CHGD             
DATAFLDS DS    XL1                 KEY FIELDS WITH DATA                         
DFLCLTQ  EQU   X'80'               CLIENT                                       
DFLPUBQ  EQU   X'40'               PUB                                          
DFLIO#Q  EQU   X'20'               IO#                                          
DFLRV#Q  EQU   X'10'               REVISION #                                   
DFLPERQ  EQU   X'08'               PERIOD                                       
DFLRDTQ  EQU   X'04'               RUN DATE                                     
DFLPRDQ  EQU   X'02'               PRODUCT                                      
DFLSTWQ  EQU   X'01'               STEWARD OPTION                               
*                                                                               
SVEIOSTA DS    XL(L'PWIOSTAT)                                                   
SVEIOADC DS    XL(L'PWIOADCD)                                                   
*                                                                               
NXTIOKYA DS    A                   A(NEXT IOKEY FROM LINK)                      
*                                                                               
LNKSTSW  DS    XL1                 STATUS PROCESSTING SWITCH                    
*                                    C'1' - FIRST IO KEY FROM LINK              
*                                    C'2' - SECOND OR LATER IO KEY              
*                                    C'L' - LAST IOKEY IN REQUEST               
*                                                                               
LNKREPCD DS    XL2                 LINK REPLY RECORD CODE                       
*                                                                               
QLMED    DS    XL(L'QMED)          MEDIA   FILTER SAVEAREA                      
QLCLT    DS    XL(L'QCLT)          CLIENT  FILTER SAVEAREA                      
QLPRD    DS    XL(L'QPRD)          PRODUCT FILTER SAVEAREA                      
QLPUB    DS    XL(L'QPUB)          PUB     FILTER SAVEAREA                      
BSER#TB  DS    500PL5              BUY SEQ# TABLE                               
*                                                                               
         ORG                                                                    
*                                                                               
*PRGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE         PRINT SYSTEM RECORD LAYOUTS                  
         PRINT ON                                                               
*DDMINBLK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMINBLK          MINIO CONTROL BLOCK                          
         PRINT ON                                                               
*PPERREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPERREQUS         PRINT SYSTEM RECORD LAYOUTS                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE PTRACLTPP         CLT TRAFFIC OFFICE CODE PASSIVE PTR          
         EJECT                                                                  
*                                                                               
       ++INCLUDE POFFCLTPP         CLT OFFICE CODE PASSIVE PTR                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPGENMATP         MAT= REPEAT PASSIVE PTR                      
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
***DDPERVALD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
*                                                                               
** FAGETTXTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
*                                                                               
***PPMAPEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE PPMAPEQUS                                                      
         PRINT ON                                                               
*                                                                               
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
**PAN#1  DC    CL21'008PPWIO10   12/17/07'                                      
         END                                                                    
