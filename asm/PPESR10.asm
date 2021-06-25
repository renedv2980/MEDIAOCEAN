*          DATA SET PPESR10    AT LEVEL 022 AS OF 03/26/08                      
*PHASE T42010A                                                                  
*                                                                               
         TITLE 'T42010 - ENHANCED SR HEADER MAINT/LIST'                         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               CHANGE LOG                                            *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 2                                                                
*                                                                               
*BOBY    JUN/05 BIG BANG                                                        
*                                                                               
         TITLE 'T42010 - ENHANCED SR HEADER MAINT/LIST'                         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               T42010 - ENHANCED SR HEADER MAINT/LIST *                        
*                                                                     *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T42000 (ESR CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, CHG, DISP, SEL, LIST, REP, DEL, RES     *         
*                        ABDEL                                        *         
*                                                                     *         
*  INPUTS       SCREEN T420FD (MAINTENANCE)                           *         
*               SCREEN T420FE (LIST)                                  *         
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
         TITLE 'T42010 - ENHANCED SR HEADER MAINT/LIST '                        
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T42010   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T42010,RR=RE                                                   
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
         MVC   QRECORD,=CL8'ESR'                                                
*                                                                               
         TITLE 'T42010 - ENHANCED SR HEADER MAINT/LIST - CKMODE'                
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
         TITLE 'T42010 - ENHANCED SR HEADER MAINT/LIST - VK'                    
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
         LHI   RF,E#SRDLRP         SET APPROPRIATE RETURN CODE                  
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
*        VALIDATE SR#                                                           
*                                                                               
VKSR#    DS    0H                                                               
*                                                                               
         LA    R2,HDRSR#H          POINT TO ENHANCED SR FIELD                   
*                                                                               
         XC    QSR#,QSR#           INIT SR NUMBER                               
*                                                                               
         CLI   FLDILEN,0           OKAY IF SR# MISSING                          
         BE    VKSR#X                                                           
*                                                                               
         GOTOR VALSR#              VALIDATE ENHANCED SR #                       
*                                                                               
         OI    DATAFLDS,DFLSR#Q    SR#    ENTERED                               
*                                                                               
VKSR#X   DS    0H                                                               
*                                                                               
*        VALIDATE REVISION #                                                    
*                                                                               
VKRV#    DS    0H                                                               
*                                                                               
         LA    R2,HDRREV#H         POINT TO REVISION FIELD                      
*                                                                               
         XC    QREV#,QREV#         INIT REVISION NUMBER                         
*                                                                               
         CLI   FLDILEN,0           OKAY IF SR# MISSING                          
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
         TM    DATAFLDS,DFLSR#Q+DFLRV#Q IF SR# AND REVISION GIVEN               
         BO    VKKEY                       GO BUILD KEY                         
*                                                                               
         TM    DATAFLDS,DFLRV#Q    IF RV# WITHOUT SR#                           
         BO    VKRV#ER                ERROR                                     
*                                                                               
         TM    DATAFLDS,DFLSR#Q    IF SR# WITHOUT RV#                           
         BNO   VK100                                                            
*                                                                               
         CLI   ACTNUM,ACTDEL       ASSUME ORIGINAL IO IF DELETING               
         BE    VKKEY                                                            
         CLI   ACTNUM,ACTABDEL                                                  
         BE    VKKEY                                                            
*                                                                               
         GOTOR FNDRV#,DMCB,QSTART  FIND MOST RECENT RV#                         
         BNZ   VKSR#2ER            IO NOT ON FILE                               
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF NOT ADDING                           
         BNE   VK60                                                             
*                                                                               
*        BUMP REVISION #                                                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,QREV#            GET RV#                                      
*                                                                               
         TM    ESRDCNTL-ESRKEY+QSRKEY,ESRDDELQ SKIP IF DELETED (REUSE)          
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
VK100    DS    0H                  NO SR# OR RV#                                
*                                                                               
         TM    DATAFLDS,DFLPERQ    MUST HAVE PERIOD                             
         BNO   VKPERER                                                          
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF NOT ADDING A NEW IO                  
         BNE   VK200                                                            
*                                                                               
         GOTOR NXTSR#              FIND NEXT SR#                                
*                                                                               
         LA    R2,HDRSR#H          POINT TO REVISION FIELD                      
*                                                                               
         GOTOR DISSR#              DISPLAY THE NEW SR#                          
*                                                                               
         MVI   QREV#,0             NO REVISION #                                
*                                                                               
         LA    R2,HDRREV#H         POINT TO REVISION FIELD                      
*                                                                               
         GOTOR DISRV#              DISPLAY THE NEW SR#                          
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
*        FIND SPACE RESERVATION THAT FITS                                       
*                                                                               
         GOTOR FNDSR#,DMCB,QSTART                                               
         BNZ   VKSR#2ER            SPACE RESERVATION NOT ON FILE                
*                                                                               
         LA    R2,HDRSR#H          POINT TO SR NUMBER FIELD                     
*                                                                               
         GOTOR DISSR#              DISPLAY SR#                                  
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
         LA    R4,KEY              ESTABLISH ENHANCED SR KEY                    
         USING ESRKEY,R4                                                        
*                                                                               
         MVC   ESRKAGY,QAGY        SET AGENCY                                   
         MVC   ESRKMED,QMED        SET MEDIA                                    
         MVI   ESRKRCD,ESRKRCDQ    SET RECORD CODE                              
         MVC   ESRKCLT,QCLT        SET CLIENT                                   
         MVC   ESRKPUB,QPUB        SET PUB CODE                                 
*                                                                               
         MVC   ESRKSRYR,QSR#SRYR   SET PERIOD YEAR                              
         MVC   ESRKSRSQ,QSR#SRSQ   SET SEQUENCE NUMBER                          
         MVC   ESRKRV#,QREV#       SET REVISION NUMBER                          
         MVC   ESRKELMK,=8X'FF'    ONLY INTERESTED IN MASTER KEYS               
*                                                                               
         OI    GENSTAT1,OKADDEL    OKAY TO ADD DELETED RECORDS                  
*                                                                               
         MVC   QSRKEY,KEY          SAVE KEY INTO QSRKEY                         
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTOR HIGH                READ MATER RECORD                            
*                                                                               
         CLC   ESRKEY,KEYSAVE      IF RECORD NOT FOUND                          
         BE    VKKEY20                                                          
*                                                                               
         CLI   ACTNUM,ACTADD          ERROR IF NOT ADDING                       
         BNE   VKSR#2ER                                                         
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
         CLI   DDLNKSW,C'Y'        SKIP IF ADBUYER DELETE                       
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
VKSR#ER  LHI   RF,PPEFLDNE        ENHANCED SR REQUIRED                          
         B     VKERR                                                            
*                                                                               
VKRV#ER  LHI   RF,PPEFLDNE        REVISION INVALID                              
         B     VKERR                                                            
*                                                                               
VKSR#2ER LHI   RF,PPEESRNF        ENHANCED SR NOT ON FILE                       
         B     VKERR                                                            
*                                                                               
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
         TITLE 'PPESR10 - PRINT NEW INSORD CONTROLLER - ABDEL'                  
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
         TITLE 'PPESR00 - PRINT NEW INSORD CONTROLLER - FILLKEY'                
***********************************************************************         
*                                                                     *         
*        READ NEXT IOKEY AND FILL IN KEY FIELDS                       *         
*                                                                     *         
* LNKESRKY  -  LONG FORM OF ESR KEY                                   *         
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
         CLC   WKDTMPCD,=AL2(D#ESRLKY)  MUST BE FOR IOKEY                       
         BNE   FSCRDONE                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WKDTRLEN       GET DATA LENGTH                              
         LR    R0,RF               SAVE ELEMENT LENGTH                          
         SHI   RF,WKDTHDLQ         SUBTRACT HEADER LENGTH                       
         BZ    FSCRSR#X              NO DATA                                    
*                                                                               
         CHI   RF,L'LNKESRKY       ERROR IF TOO LONG                            
         BH    FSCRSR#X                                                         
*                                                                               
         XC    LNKESRKY,LNKESRKY   INIT SAVEAREA                                
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LNKESRKY(0),WKDTDATA MOVE TEXT TO WORK FIELD                     
*                                                                               
         OC    LNKESRKY,LNKESRKY                                                
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE ESR KEY!                           
*                                                                               
         SR    R2,R2                                                            
         GOTOR PRSSR#,DMCB,(L'LNKESRKY,LNKESRKY)  BREAK OUT KEY                 
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
         MVC   HDRSR#,SPACES                                                    
*                                                                               
         LA    R2,HDRSR#H          POINT TO SPACE RESERVATION # FIELD           
*                                                                               
         GOTOR DISSR#              DISPLAY IT                                   
*                                                                               
         LA    R2,HDRREV#H         POINT TO REVISION # FLD                      
*                                                                               
         GOTOR DISRV#              DISPLAY IT                                   
*                                                                               
         B     FSCRSR#X                                                         
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,QSR#SRYR       GET SR# YEAR                                 
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  HDRSR#(2),DUB                                                    
*                                                                               
         ICM   RF,7,QSR#SRSQ       GET SR# SEQ NUMBER                           
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  HDRSR#+2(4),DUB                                                  
*                                                                               
         MVI   HDRSR#H+5,6                                                      
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
FSCRSR#X DS    0H                                                               
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
         TITLE 'PPESR10 - PRINT NEW INSORD CONTROLLER - GETINPUT'               
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
         TITLE 'T42010 - ENHANCED SR HEADER MAINT/LIST - VR'                    
***********************************************************************         
*                                                                     *         
*        VALIDATE ENHANCED SR HEADER FIELDS                           *         
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
         USING ESRKEY,R4                                                        
*                                                                               
         TM    ESRDCNTL,ESRDDELQ   IF RECORD IS DELETED                         
         BNO   VRADDX                                                           
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO CONTROL BLOCK                
         USING MINBLKD,R7                                                       
*                                                                               
         MVC   MINMKEY,ESRKEY      COPY MASTER KEY                              
*                                  NEED TO REUSE MINIO SET                      
         GOTOR VMINIO,ESRPARMS,('MINOPN',MINBLKD)  OPEN  MINIO SET              
         GOTOR VMINIO,ESRPARMS,('MINRSF',MINBLKD)  RESTORE                      
         GOTOR VMINIO,ESRPARMS,('MINCLS',MINBLKD)  CLOSE MINIO SET              
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
         MVC   QRDATE,PVALBSTA     SAVE ENHANCED SR RUN DATE                    
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
         LA    R4,MINMKEY          ESTABLISH ENHANCED SR MASTER KEY             
         USING ESRKEY,R4                                                        
*                                                                               
         CLI   ACTNUM,ACTADD       CHECK IF ADDING                              
         BE    VR90                                                             
*                                                                               
         MVC   ESRKEY,QSRKEY       IF NOT ADD THEN WE ALREADY HAVE              
*                                  KEY FROM VK                                  
         B     VR95                CONTINUE WITH OPEN MINIO                     
*                                                                               
VR90     DS    0H                                                               
*                                                                               
         OI    QACTCHG1,ESRAADD    INDICATE ELEMENT ADDED                       
*                                                                               
         MVC   ESRKAGY,QAGY        SET AGENCY                                   
         MVC   ESRKMED,QMED        SET MEDIA                                    
         MVI   ESRKRCD,ESRKRCDQ    SET RECORD CODE                              
         MVC   ESRKCLT,QCLT        SET CLIENT                                   
         MVC   ESRKPUB,QPUB        SET PUB                                      
         MVC   ESRKSR#,QSR#        SET SR#                                      
         MVC   ESRKRV#,QREV#       SET REVISION #                               
         MVC   QSRKEY,ESRKEY       SAVE MASTER KEY                              
*                                                                               
VR95     DS    0H                                                               
*                                                                               
*        OPEN MINIO SET                                                         
*                                                                               
         MVI   MINDELSW,C'Y'       OPEN MINIO SET                               
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
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
         USING ESRHDRD,R6                                                       
*                                                                               
         MVI   ESRHKCDE,ESRHKIDQ    HEADER ELEMENT CODE                         
         MVI   ESRHKLEN,ESRHDRLQ    HEADER ELEMENT LENGTH                       
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF ADD                                  
         BE    VR150                                                            
*                                  NOT ADD FUNCTION                             
         TM    MINSTAT,MINDELQ     RECORD CAN'T BE DELETED                      
         BO    VRCNTCHG                                                         
*                                                                               
         GOTOR GETELM,DMCB,ESRHKEY  FIND ELEMENT IN RECORD                      
*                                                                               
         ICM   R6,15,MINELEM        POINT R6 TO FOUND ELEMENT                   
         BNZ   *+6                                                              
         DC    H'0'                MUST FIND HEADER                             
*                                                                               
VR150    DS    0H                                                               
*                                                                               
         USING ESRHDRD,R6                                                       
*                                                                               
         MVC   ESRHDATE,QRDATE      ENHANCED SR DATE - BINARY                   
         MVC   ESRHSTAT,HDRSTA      ENHANCED SR STATUS                          
         MVC   ESRHSTRT,BSTART      PERIOD START DATE - BI                      
         MVC   ESRHEND,BEND         PERIOD END   DATE - BI                      
         MVC   ESRHPRD,QPRD         PRODUCT                                     
         MVC   ESRHEST,BEST         ESTIMATE IN BINARY                          
*                                                                               
         OC    ESRHEST,ESRHEST     IF THERE IS AN ESTIMATE                      
         BZ    *+8                                                              
         OI    ESRHOTYP,ESRTESTQ      SET FLAG                                  
*                                                                               
         MVC   ESRHSTEW,QSTEW       STEWARDSHIP                                 
*                                                                               
VR175    DS    0H                                                               
*                                                                               
         L     RF,WRTELM           ASSUMING RE-WRITING ELEMENT                  
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADD                                       
         BNE   *+8                                                              
         L     RF,ADDELM              USE DIFFERENT ROUTINE                     
*                                                                               
         GOTOR (RF),DMCB,ESRHKEY   WRITE ELEMENT TO RECORD                      
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
         GOTOR VMINIO,ESRPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
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
         TITLE 'T42010 - ENHANCED SR HEADER MAINT/LIST - DR'                    
***********************************************************************         
*                                                                     *         
*        DISPLAY ENHANCED SR HEADER RECORD                            *         
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
         LA    R4,MINMKEY          ESTABLISH ENHANCED SR MASTER KEY             
         USING ESRKEY,R4           DISPLAY ACTIVE USING                         
*                                                                               
         MVC   ESRKEY,QSRKEY       SHOULD HAVE KEY OF MASTER KEY                
         MVC   QSR#,ESRKSR#                                                     
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
* OPEN MINIO SET                                                                
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0             SHOULD BE NO ERROR                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,ELEMENT           BUILD ELM                                   
         XC    ELEMENT,ELEMENT      CLEAR                                       
         USING ESRHDRD,R6                                                       
*                                                                               
         MVI   ESRHKEY,ESRHKIDQ     ELEM ID                                     
*                                                                               
         GOTOR GETELM,DMCB,ESRHKEY  GET ELEMENT TO RECORD                       
*                                                                               
         ICM   R6,15,MINELEM        POINT R6 TO ELEMENT                         
         BNZ   *+6                  CHECK IF ITS THERE                          
         DC    H'0'                                                             
*                                                                               
*        DISPLAY MEDIA                                                          
*                                                                               
         LA    R2,HDRMEDH                                                       
         MVC   QMED,ESRKMED                                                     
         GOTOR DISMED              DISPLAY MEDIA                                
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
*        DISPLAY CLIENT                                                         
*                                                                               
         LA    R2,HDRCLTH                                                       
         MVC   QCLT,ESRKCLT                                                     
         GOTOR DISCLT              DISPLAY CLIENT                               
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
*        DISPLAY IO #                                                           
*                                                                               
         LA    R2,HDRSR#H                                                       
         MVC   QSR#,ESRKSR#                                                     
         GOTOR DISSR#                                                           
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
*        DISPLAY RUN DATE                                                       
*                                                                               
         LA    R2,HDRDTEH                                                       
         BRAS  RE,CLRFLD           INIT FIELD                                   
         XC    HDRDTE,HDRDTE       ENHANCED SR DATE                             
         GOTO1 DATCON,DMCB,(3,ESRHDATE),(11,HDRDTE)                             
         OI    HDRDTEH+6,X'80'                                                  
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
*        DISPLAY STATUS                                                         
*                                                                               
         LA    R2,HDRSTAH                                                       
         MVC   QSTAT,ESRHSTAT                                                   
         GOTOR DISSTA                                                           
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
*        DISPLAY PERIOD                                                         
*                                                                               
         LA    R2,HDRPERH                                                       
         MVC   QPER,ESRHSTRT                                                    
         GOTOR DISPER,DMCB,QPER    DISPLAY PERIOD                               
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
*        DISPLAY EXPANDED SR#                                                   
*                                                                               
         LA    R2,HDRSR#NH         EXPANDED SR#                                 
*                                                                               
         BRAS  RE,CLRFLD           CLEAR THE FIELD                              
*                                                                               
         GOTOR FMTSR#              FORMAT THE SR#                               
*                                                                               
         MVC   FLDDATA(L'QSR#EXP),QSR#EXP DISPLAY SR#                           
*                                                                               
*        DISPLAY PRODUCT                                                        
*                                                                               
         LA    R2,HDRPRDH                                                       
         MVC   QPRD,ESRHPRD                                                     
         GOTOR DISPRD              DISPLAY PRODUCT                              
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
*        DISPLAY STEWARD OPTION                                                 
*                                                                               
         LA    R2,HDRSTEWH                                                      
         MVC   QSTEW,ESRHSTEW                                                   
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OLD DATA                               
*                                                                               
         MVC   FLDDATA(L'QSTEW),QSTEW                                           
*                                                                               
*        DISPLAY ESTIMATE                                                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,ESRHEST        GET ESTIMATE                                 
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
         GOTOR VMINIO,ESRPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
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
         TITLE 'T42010 - ENHANCED SR HEADER MAINT/LIST - DK'                    
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
         USING ESRKEY,R4           ESTABLISH MASTER KEY                         
*                                                                               
         CLI   ACTNUM,ACTABDEL     OR ACTION ABDELETE                           
         BNE   DK00                                                             
*                                                                               
         MVC   ESRKAGY,QAGY           SET AGENCY CODE                           
         MVC   ESRKMED,QMED           SET MEDIA                                 
         MVI   ESRKRCD,ESRKRCDQ       SET RECORD CODE                           
         MVC   ESRKCLT,QCLT           SET CLIENT                                
         MVC   ESRKPUB,QPUB           SET PUB                                   
         MVC   ESRKSR#,QSR#           SET ENHANCED SR SERIAL NUMBER             
         MVC   ESRKRV#,QREV#          SET ENHANCED SR REVISION NUMBER           
*                                                                               
         B     DK05                                                             
*                                                                               
DK00     DS    0H                                                               
*                                                                               
         MVC   MINMKEY(L'ESRKEY),KEY  MOVE KEY TO MASTER KEY                    
         MVC   QSRKEY(L'ESRKEY),KEY   SAVE KEY  IN Q AREA                       
*                                                                               
DK05     DS    0H                                                               
*                                                                               
* OPEN MINIO SET                                                                
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        SAVE KEY FIELDS                                                        
*                                                                               
         MVC   QMED,ESRKMED                                                     
         MVC   QCLT,ESRKCLT                                                     
         MVC   QPUB,ESRKPUB                                                     
         MVC   QSR#,ESRKSR#                                                     
         MVC   QREV#,ESRKRV#                                                    
*                                                                               
         LA    R6,ELEMENT           BUILD ELM                                   
         XC    ELEMENT,ELEMENT      CLEAR                                       
         USING ESRHDRD,R6                                                       
*                                                                               
         MVI   ESRHKEY,ESRHKIDQ     ELEM ID                                     
*                                                                               
         GOTOR GETELM,DMCB,ESRHKEY  GET ELEMENT TO RECORD                       
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
         LA    R2,HDRSR#H          POINT TO SR# FIELD HEADER                    
*                                                                               
         GOTOR DISSR#              DISPLAY SR#                                  
                                                                                
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
         TITLE 'T42010 - ENHANCED SR HEADER MAINT/LIST - LR'                    
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
         USING ESRKEY,R4           ESTABLISH RECORD KEY                         
*                                  SET UP INITIAL KEY                           
         MVC   ESRKAGY,QAGY        AGENCY                                       
         MVC   ESRKMED,QMED        MEDIA                                        
         MVI   ESRKRCD,ESRKRCDQ    RECORD ID                                    
*                                                                               
         MVC   QLCLT,QCLT          COPY FILTERS                                 
         MVC   QLPUB,QPUB                                                       
         MVC   QLPRD,QPRD                                                       
*                                                                               
         CLC   QLCLT,=8X'FF'        IF FILTERING ON CLIENT                      
         BE    *+10                                                             
         MVC   ESRKCLT,QLCLT           SET STARTING CLIENT                      
*                                                                               
         CLC   QLPUB,=8X'FF'        IF FILTERING ON PUB                         
         BE    *+10                                                             
         MVC   ESRKPUB,QLPUB        STARTING PUB                                
*                                                                               
LRKEYX   DS    0H                                                               
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTOR HIGH                FIRST RECORD                                 
*                                                                               
LRLOOP   DS    0H                                                               
*                                                                               
         CLC   ESRKEY(ESRKCLT-ESRKEY),KEYSAVE AGY/MED/RECORD CODE?              
         BNE   LRDONE                                                           
*                                                                               
         CLC   QLCLT,=8X'FF'        SKIP IF NOT FILTERING ON CLIENT             
         BE    LRFLCLTX                                                         
         OC    QLCLT,QLCLT           SKIP IF NOT FILTERING ON CLIENT            
         BZ    LRFLCLTX                                                         
*                                                                               
******   CLC   ESRKCLT,QLCLT        DONE ON CHANGE IN CLIENT                    
******   BNE   LRDONE                                                           
*                                                                               
LRFLCLTX DS    0H                                                               
*                                                                               
         CLC   QLPUB,=8X'FF'        SKIP IF NOT FILTERING ON PUB                
         BE    LRFLPUBX                                                         
         OC    QLPUB,QLPUB           SKIP IF NOT FILTERING ON PUB               
         BZ    LRFLPUBX                                                         
*                                                                               
*******  CLC   ESRKPUB,QLPUB        SKIP IF WRONG PUB                           
*******  BNE   LRCONT                                                           
*                                                                               
LRFLPUBX DS    0H                                                               
*                                                                               
         CLC   ESRKELMK,=8X'FF'    SKIP IF NOT MASTER KEY                       
         BNE   LRCONT                                                           
*                                                                               
         TM    ESRDCNTL,ESRDDELQ   SKIP IF DELETED                              
         BO    LRCONT                                                           
*                                                                               
         MVC   DMDSKADD,ESRDDISK   SAVE DISK ADDR FOR LIST                      
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO CONTROL BLOCK                
         USING MINBLKD,R7                                                       
*                                                                               
         MVC   MINMKEY(L'ESRKEY),ESRKEY                                         
*                                                                               
*        OPEN MINIO SET                                                         
*                                                                               
         MVI   MINDELSW,C'Y'       READ FOR DELETES                             
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                NO ERRORS TOLERATED                          
*                                                                               
         LA    R4,MINMKEY          POINT TO IO MASTER KEY                       
*                                                                               
         MVC   QMED,ESRKMED        SAVE MEDIA                                   
         MVC   QCLT,ESRKCLT        CLIENT                                       
         MVC   QPUB,ESRKPUB        PUB                                          
         MVC   QSR#,ESRKSR#        SR#                                          
         MVC   QREV#,ESRKRV#       REVISION #                                   
*                                                                               
*        READ HEADER ELEMENT                                                    
*                                                                               
         LA    R6,ELEMENT           BUILD ELM                                   
         XC    ELEMENT,ELEMENT      CLEAR                                       
         USING ESRHDRD,R6                                                       
*                                                                               
         MVI   ESRHKEY,ESRHKIDQ     ELEM ID                                     
*                                                                               
         GOTOR GETELM,DMCB,ESRHKEY  GET ELEMENT TO RECORD                       
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
         MVC   DLISTCLT,ESRKCLT     CLIENT                                      
*                                                                               
LRPUB    DS    0H                                                               
*                                                                               
         MVC   LSPUB,SPACES        INIT PUB FIELD                               
*                                                                               
         GOTO1 VPUBEDIT,DMCB,(0,ESRKPUB),(C'S',LSPUB)                           
*                                                                               
         CLI   0(R1),X'FF'         CHECK FOR ERRORS                             
         BNE   *+6                                                              
         DC    H'0'                SHOULD HAVE VALID PUB CODE                   
*                                                                               
         MVC   DLISTPBN,SPACES     INIT PUB NAME                                
*                                                                               
         GOTOR DISPBNM,DMCB,ESRKPUB,DLISTPBN                                    
*                                                                               
         LA    R1,DLISTPBN+L'DLISTPBN-1 LAST BYTE OF NAME FIELD                 
         LHI   R0,L'DLISTPBN       MAX NUMBER OF BYTES IN NAME FLD              
*                                                                               
         CLI   0(R1),C' '          FIND LAST OF NAME                            
         BH    *+10                                                             
         BCTR  R1,0                BACK UP A BYTE                               
         BCT   R0,*-10                                                          
*                                                                               
         AHI   R1,1                NEXT BYTE AFTER NAME                         
*                                                                               
         LHI   RF,L'DLISTPBN       FIELD LENGTH                                 
         SR    RF,R0               BYTES LEFT IN FIELD                          
         BZ    LRPUBX                                                           
*                                                                               
         MVI   0(R1),C'('                                                       
         SHI   RF,1                ADJUSTAVAILABLE LENGTH                       
         BZ    LRPUBX                                                           
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R1),LSPUB       PUB NUMBER                                   
*                                                                               
         LA    R1,DLISTPBN+L'DLISTPBN-1 LAST BYTE OF NAME FIELD                 
         LHI   R0,L'DLISTPBN       MAX NUMBER OF BYTES IN NAME FLD              
*                                                                               
         CLI   0(R1),C' '          FIND LAST OF NAME                            
         BH    *+14                                                             
         BCTR  R1,0                BACK UP A BYTE                               
         BCT   R0,*-10                                                          
         B     LRPUBX              NO MORE ROOM                                 
*                                                                               
         MVI   1(R1),C')'                                                       
*                                                                               
LRPUBX   DS    0H                                                               
*                                                                               
*        DISPLAY SR#                                                            
*                                                                               
         GOTOR FMTSR#              FORMAT  IO #                                 
*                                                                               
         MVC   DLISTSR#,QSR#EXP    ADD SR# TO LIST                              
*                                                                               
*****    MVC   DLISTPRD,ESRHPRD     PRODUCT                                     
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
         GOTO1 DATCON,ESRPARMS,(X'93',ESRHSTRT),(17,DLISTPER)                   
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINCLS',MINBLKD)  CLOSE MINIO SET              
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
         TITLE 'T42010 - ENHANCED SR HEADER MAINT/LIST - PR'                    
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
         TITLE 'T42010 - ENHANCED SR HEADER MAINT/LIST - RS'                    
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
         LA    R4,MINMKEY          ESTABLISH ENHANCED SR MASTER KEY             
         USING ESRKEY,R4                                                        
*                                                                               
         MVC   ESRKEY,QSRKEY       MOVE MASTER KEY                              
*                                                                               
*        OPEN  MINIO                                                            
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
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
         GOTOR VMINIO,ESRPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
*                                                                               
*        RE-OPEN AND CREATE ACTIVITY ELEM                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH ENHANCED SR MASTER KEY             
         USING ESRKEY,R4                                                        
*                                                                               
         MVC   ESRKEY,QSRKEY       MOVE MASTER KEY                              
*                                                                               
*        OPEN  MINIO                                                            
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
*        CREATE RESTORE ACTIVITY ELEMENT FOR ENHANCED SR                        
*                                                                               
*                                                                               
         OI    QACTCHG1,ESRARST    INDICATE RESTORE TOOK PLACE                  
*                                                                               
         GOTOR ACTPUT              ADD ACTIVITY ELEMENT                         
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
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
         TITLE 'T42010 - ENHANCED SR HEADER MAINT/LIST - GETEL'                 
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
         TITLE 'T42010 - ENHANCED SR HEADER MAINT/LIST - DISPBNM'               
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
         MVC   0(L'PUBNAME,R5),PUBNAME                                          
*                                                                               
DISPBNMX DS    0H                                                               
*                                                                               
*        RESTORE FILE POINTERS                                                  
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY(L'MYKEY),MYKEY  RESTORE KEY                                  
         GOTO1 HIGH                                                             
         LA    R4,KEY                                                           
         USING ESRKEY,R4           ESTABLISH ESR RECORD KEY                     
         MVC   DMDSKADD,ESRDDISK   SAVE CURRENT DISK ADDRESS                    
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T42010 - ENHANCED SR HEADER MAINT/LIST - RDEL'                  
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
         LA    R7,MNBLKCB          ESTABLISH MINIO CONTROL BLOCK                
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH ENHANCED SR MASTER KEY             
         USING ESRKEY,R4                                                        
*                                                                               
         MVC   ESRKEY,QSRKEY       MOVE MASTER KEY                              
*                                                                               
*        OPEN  MINIO                                                            
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
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
         GOTOR VMINIO,ESRPARMS,('MINHI',MINBLKD)                                
*                                                                               
         CLI   MINERR,0                                                         
         BNE   RDELDONE            END OF MINIO SET                             
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
         USING ESRBUYD,R6          ESTABLISH BUY ELEMENT                        
*                                                                               
         CLI   ESRBKCDE,ESRBKIDQ   IF BUY SERIAL NUMBER ELEMENT                 
         BNE   RDELBUYX                                                         
*                                                                               
         MVC   0(5,R2),ESRBSER#    SAVE SERIAL NUMBER                           
*                                                                               
         LA    R2,5(R2)               BUMP TO NEXT SERIAL# SAVEAREA             
         XC    0(5,R2),0(R2)          INITIALIZE                                
*                                                                               
RDELBUYX DS    0H                                                               
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINDEL',MINBLKD)  DELETE ELEMENT               
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
         GOTOR VMINIO,ESRPARMS,('MINCLS',MINBLKD) CLOSE   MINIO SET             
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINOPN',MINBLKD) RE-OPEN MINIO SET             
*                                                                               
*        DELETE MINIO SET                                                       
*                                                                               
         GOTO1 VMINIO,DMCB,('MINDLF',(R7))                                      
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        DELETE ESR ELEMENTS FROM BUYS                                          
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
         MVI   ELCODE,PESRELCQ     SET TO FIND ESR ELEMENT                      
         L     R6,AIO              POINT TO FOUND RECORD                        
*                                                                               
         BRAS  RE,GETEL            FIND SERIAL NUMBER ELEMENT                   
*                                                                               
RDELSRLP DS    0H                                                               
*                                                                               
         BNE   RDELSRDN            END OF RECORD                                
*                                                                               
         USING PESRELEM,R6         ESTABLISH ESR ELEMENT                        
*                                                                               
         CLC   PESRNUMB,QSR#       MATCH ON IO NUMBER                           
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
         GOTOR VRECUP,DMCB,(X'01',AIO),PESRELEM,0    DELETE ELM                 
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
         CLI   PCHGELEM,PESRELCQ   DONE IF NEXT IO ELM FOUND                    
         BE    RDELCHGD                                                         
*                                                                               
         CLI   PCHGELEM,PCHGELQ    SKIP IF NOT A CHGELEM                        
         BNE   RDELCHGC                                                         
*                                                                               
         IC    RF,PCHGLEN          GET ELEMENT LENGTH                           
*                                                                               
         CHI   RF,PCHGNEWS         IF ELEMENT HAS PID IN IT                     
         BNE   *+12                                                             
         LA    R1,PCHG_XSS            POINT TO ELEMENT EXTENSION                
         B     RDELCHG1                                                         
*                                                                               
         CHI   RF,PCHGNEWL                                                      
         BNE   *+12                                                             
         LA    R1,PCHG_XLS            POINT TO ELEMENT EXTENSION                
         B     RDELCHG1                                                         
*                                                                               
         B     RDELCHGC            ELSE SKIP ELEMENT                            
*                                                                               
RDELCHG1 DS    0H                                                               
*                                                                               
         USING PCHGEXT,R1          ESTABLISH ELEMENT EXTENSION                  
*                                                                               
         TM    PCHGIND5,PCHGESRG   OKAY IF AN ESR SENT CHGELEM                  
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
         NI    PCHGIND5,X'FF'-PCHGESRG TURN OFF ESR GENERATED IND               
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
         CLI   PCHGIND5,0                                                       
         BNE   RDELCHGD                                                         
*                                                                               
*        NO OTHER CHANGES INDICATED IN ELEMENT                                  
*              DELETE ELEMENT                                                   
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
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ESRLKY),    X        
               ('LD_CHARQ',LNKESRKY),(L'LNKESRKY,0)                             
*                                                                               
RDELLNKX DS    0H                                                               
*                                                                               
RDELX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
DRCNTDEL LHI   RF,PPERECDL                                                      
         J     VDRECERR                                                         
*                                                                               
DRECNDEL LHI   RF,PPEDETDL         DETAILS MUST BE ALL DELETED                  
         J     VDRECERR                                                         
*                                                                               
VDRECERR DS    0H                                                               
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
         TITLE 'PPESR00 - PRINT NEW ENHANCED SR CONTROLLER - CLRFLD'            
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
         TITLE 'PPESR00 - PRINT NEW ENHANCED SR CONTROLLER - SETLEN'            
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
         TITLE 'PPESR00 - PRINT NEW ENHANCED SR CONTROLLER - BUMP'              
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
          TITLE 'T42010 - FAX MAINT/LIST - WKRDATD'                             
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
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - STATUSTD'              
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
DLISTPBN DS    CL27                PUB                                          
         DS    CL1                                                              
DLISTSR# DS    CL22                SR #                                         
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
       ++INCLUDE PPESRFFD                                                       
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPESRFED                                                       
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPESRFDD          ENHANCED SR HEADER MAINT SCREEN              
         EJECT                                                                  
*                                                                               
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPESRWRKD                                                      
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
LQSR#    DS    CL(L'QSR#)                                                       
QMONEY   DS    PL6                                                              
CHGSWTCH DS    CL1                 C'Y' - A KEY FIELD HAS BEEN CHGD             
DATAFLDS DS    XL1                 KEY FIELDS WITH DATA                         
DFLCLTQ  EQU   X'80'               CLIENT                                       
DFLPUBQ  EQU   X'40'               PUB                                          
DFLSR#Q  EQU   X'20'               SR#                                          
DFLRV#Q  EQU   X'10'               REVISION #                                   
DFLPERQ  EQU   X'08'               PERIOD                                       
DFLRDTQ  EQU   X'04'               RUN DATE                                     
DFLPRDQ  EQU   X'02'               PRODUCT                                      
DFLSTWQ  EQU   X'01'               STEWARD OPTION                               
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
**PAN#1  DC    CL21'022PPESR10   03/26/08'                                      
         END                                                                    
