*          DATA SET TAREP65    AT LEVEL 017 AS OF 09/03/19                      
*PHASE T70365A,*                                                                
*INCLUDE SMTP                                                                   
         TITLE 'T70365 - NON-DDS CLARUS'                                        
***********************************************************************         
*                                                                     *         
*        NON-DDS CLARUS UPLOAD - TAREP65                              *         
*                                                                     *         
***********************************************************************         
*                                                                               
T70365   CSECT                                                                  
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NMOD1 0,T70365                                                         
*                                                                               
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC             ESTABLISH CONTROLLER WRKSTORAGE              
*                                                                               
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA          ESTABLISH SCREEN                             
*                                                                               
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9          ESTABLISH SYSTEM W/S                         
*                                                                               
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8           ESTABLISH SPOOL CONTROL BLOCK                
*                                                                               
         LAY   R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7              R7=A(LOCAL W/S)                              
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - CKMODE'                               
***********************************************************************         
*                                                                     *         
*        MODE CONTROLLED ROUTINES                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
CKMODE   DS    0H                                                               
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   *+8                                                              
         BRAS  RE,VK               VALIDATE KEY                                 
                                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   *+8                                                              
         BRAS  RE,PREP             PRINT REPORT                                 
                                                                                
         XIT1                                                                   
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - VK'                                   
***********************************************************************         
*                                                                     *         
*        VALIDATE KEY                                                 *         
*                                                                     *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,VOPT             VALIDATE OPTIONS                             
*                                                                               
VKX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
                                                                                
FLDINV   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - VOPT'                                 
***********************************************************************         
*        VALIDATE OPTIONS                                             *         
***********************************************************************         
*                                                                               
VOPT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   TRACOPT,0           INIT                                         
         MVI   EMAILOPT,0                                                       
         XC    SVEMAIL,SVEMAIL                                                  
*                                                                               
         CLI   SPLOPTH+5,0         SKIP IF NO OPTIONS                           
         BE    VOPTX                                                            
*                                                                               
         LA    R2,SPLOPTH          POINT TO OPTIONS                             
*                                                                               
         USING SCAND,R3            ESTABLISH SCAN BLOCK                         
         LA    R3,BLOCK                                                         
*                                                                               
*        SCAN OPTIONS - UP TO 40 BYTES, SECOND FIELD                            
*                                                                               
         GOTO1 SCANNER,DMCB,(40,0(R2)),(R3),0                                   
*                                                                               
         CLI   4(R1),0             ERROR IF NONE                                
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         ZIC   R0,4(R1)            GET NUMBER OF OPTIONS                        
*                                                                               
VOPTLOOP DS    0H                                                               
*                                                                               
         CLC   =C'TRACE',SCDATA1   CHECK FOR TRACE OPTION                       
         BNE   *+12                                                             
         MVI   TRACOPT,C'Y'                                                     
         B     VOPTCONT                                                         
*                                                                               
         CLC   =C'REPORT',SCDATA1  CHECK FOR REPORT PRINT OPTION                
         BNE   *+12                                                             
         MVI   REPTOPT,C'Y'                                                     
         B     VOPTCONT                                                         
*                                                                               
         CLC   =C'RERUN',SCDATA1    CHECK FOR RERUN OPTION                      
         BNE   *+12                                                             
         MVI   WRERUNOP,C'Y'                                                    
         B     VOPTCONT                                                         
*                                                                               
         CLC   =C'EMAIL',SCDATA1   CHECK FOR EMAIL OPTION                       
         BNE   VOPTEMLN                                                         
*                                                                               
         LLC   RF,SCLEN2           EMAIL MESSAGE LENGTH                         
         LTR   RF,RF               SKIP IF NONE                                 
         BZ    VOPTCONT                                                         
*                                                                               
         MVC   SVEMAIL,SPACES      SET TO SPACES                                
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVEMAIL(0),SCDATA2     SAVE EMAIL ADDRESS                        
*                                                                               
         LA    R1,SVEMAIL(RF)      POINT TO LAST OF ADDRESS                     
         CLI   0(R1),C':'          IF NOT COLON                                 
         BE    *+8                                                              
         MVI   1(R1),C':'             ADD ONE                                   
*                                                                               
         MVI   EMAILOPT,C'Y'       INDICATE ALTERNATE EMAIL ADDRESS             
*                                                                               
VOPTEMLX DS    0H                                                               
*                                                                               
         B     VOPTCONT                                                         
*                                                                               
VOPTEMLN DS    0H                                                               
*                                                                               
VOPTCONT LA    R3,62(R3)                                                        
         BCT   R0,VOPTLOOP                                                      
*                                                                               
VOPTDONE DS    0H                                                               
*                                                                               
VOPTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - PREP'                                 
***********************************************************************         
*                                                                     *         
*        PRINT REPORT                                                 *         
*                                                                     *         
***********************************************************************         
                                                                                
PREP     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,INIT             INITIALIZE                                   
*                                                                               
*                                                                               
*        GET A(TRPACK)                                                          
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'   TRPACK                                  
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FE'                                                      
         BNE   *+6                                                              
         DC    H'00'                                                            
         MVC   ADTRPACK,0(R1)       SAVE ADDRESS OF TRPACK                      
*                                                                               
*        PROCESS UPLOAD TAPE                                                    
*                                                                               
PRLOOP   DS    0H                                                               
*                                                                               
         LAY   R1,UPLDTAPE         POINT TO DCB                                 
         GET   (1),UPLDREC         READ  FIRST/NEXT RECORD                      
*                                                                               
         AP    WRECCTR,=P'1'       BUMP BATCH RECORD COUNTER                    
*                                                                               
         LA    R3,UPLDREC          ESTABLISH FILE RECORD                        
         USING NDTWD,R3            ESTABLISH RECORD AS WILDSPOT RECORD          
*                                                                               
*        MATCH RECORD TYPE TO PROCESSING ROUTINE                                
*                                                                               
         LA    R1,ROUTTAB          POINT TO TABLE OF ROUTINES                   
*                                                                               
PRTABLP  DS    0H                                                               
*                                                                               
         CLI   0(R1),X'FF'         DONE AT END OF TABLE                         
         BE    PRTABDN                                                          
*                                                                               
         CLC   NDTWID,0(R1)        MATCH ON RECORD ID                           
         BE    PRTABFD                                                          
*                                                                               
PRTABCN  DS    0H                                                               
*                                                                               
         LA    R1,8(R1)            NEXT ROUTINE                                 
         B     PRTABLP                                                          
*                                                                               
PRTABFD  DS    0H                  ENTRY IN ROUTINE TABLE FOUND                 
*                                                                               
         ICM   RF,15,4(R1)         GET ROUTINE ADDR FROM TABLE                  
         BASR  RE,RF               PROCESS RECORD                               
*                                                                               
         B     PRCONT                                                           
*                                                                               
PRTABDN  DS    0H                                                               
*                                                                               
         LAY   R2,ERRUNKNW         UNKNOWN RECORD TYPE                          
*                                                                               
         GOTOR SMTPERR,PARMS,(0,(R2)),(0,(R3)) SEND ERROR E-MAIL                
*                                                                               
         MVI   WBYPASS,C'Y'        BYPASS REMAINDER OF BATCH                    
*                                                                               
PRCONT   DS    0H                                                               
*                                                                               
         B     PRLOOP              GO READ NEXT RECORD                          
         B     PRLOOP              NEXT BATCH                                   
*                                                                               
*        END OF BATCH INPUT                                                     
*                                                                               
PRDONE   DS    0H                                                               
*                                                                               
*        CLOSE UPLOAD TAPE                                                      
*                                                                               
         LAY   R2,UPLDTAPE                                                      
         CLOSE ((2))                                                            
*                                                                               
PRSMTP   DS    0H                                                               
*                                                                               
*        CLOSE DOWN SMTP                                                        
*                                                                               
         CLI   WSMTPSW,X'FF'       SKIP IF SMTP NOT USED                        
         BNE   PRSMTPX                                                          
*                                                                               
         GOTOR =V(SMTP),PARMS,('SMTPASND',0)  SEND E-MAIL                       
*                                                                               
         GOTOR =V(SMTP),PARMS,('SMTPAEND',0)  CLOSE SMTP                        
*                                                                               
         MVI   WSMTPSW,0           CLEAR SWITCH                                 
*                                                                               
PRSMTPX  DS    0H                                                               
*                                                                               
PREPX    DS    0H                                                               
         MVI   MODE,RUNLAST        END PROGRAM                                  
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
*        TABLE OF ROUTINES TO PROCESS RECORD TYPES                              
*                                                                               
*        DC    AL1(RECORD ID),AL3(ROUTINE)                                      
*                                                                               
ROUTTAB  DS    0F                                                               
         DC    AL3(NDHDIDQ),AL1(0),AL4(PRHD) HEADER                             
         DC    AL3(NDTWIDQ),AL1(0),AL4(PRTW) TV WILD SPOTS                      
         DC    AL3(NDRWIDQ),AL1(0),AL4(PRRW) RADIO WILD SPOTS                   
         DC    AL3(NDLCIDQ),AL1(0),AL4(PRLC) LOCAL CABLE                        
         DC    AL3(NDCBIDQ),AL1(0),AL4(PRCB) CABLE SYSTEMS                      
         DC    AL3(NDNTIDQ),AL1(0),AL4(PRNT) NETWORK                            
         DC    AL3(NDTRIDQ),AL1(0),AL4(PRTR) TRAILER                            
*                                                                               
         DC    X'FF'               EOT                                          
                                                                                
         TITLE 'T70365 - NON-DDS CLARUS - PRHD'                                 
***********************************************************************         
*                                                                     *         
*        FILE HEADER RECORD                                           *         
*                                                                     *         
*NTRY    R3==> UPLOAD RECORD                                          *         
*                                                                     *         
***********************************************************************         
                                                                                
PRHD     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZAP   WERRCTR,=P'0'       INIT ERROR  COUNTER                          
*                                                                               
*        CLOSE CURRENT WORKER FILE                                              
*                                                                               
         CLI   WWRKRSW,X'FF'       IF WORKER FILE OPEN                          
         BNE   PRHDWRKX               ERROR - MISSING TRAILER RECORD            
*                                                                               
         LAY   R2,ERRTRLE          SET ERROR MESSAGE                            
*                                                                               
         GOTOR SMTPERR,PARMS,(0,0(R2)),(0,SVHDR)  E-MAIL ERROR                  
*                                                                               
         BRAS  RE,CLOSWRKR            CLOSE WORKER FILE                         
*                                                                               
         MVI   WWRKRSW,X'FF'          TURN OFF OPEN SWITCH                      
*                                                                               
PRHDWRKX DS    0H                                                               
*                                                                               
*        CLOSE DOWN THE SORT                                                    
*                                                                               
         CLI   WSORTSW,X'FF'       IF SORT ALREADY OPEN                         
         BNE   PRHDSRTX                                                         
*                                                                               
         GOTOR SORTER,DMCB,=C'END'    CLOSE DOWN SORT                           
*                                                                               
         MVI   WSORTSW,0              TURN OFF SWITCH                           
*                                                                               
PRHDSRTX DS    0H                                                               
*                                                                               
         MVI   WBYPASS,0           INIT  BYPASS SWITCH                          
         ZAP   WRECCTR,=P'1'       RESET RECORD COUNTER                         
*                                                                               
         USING NDHDD,R3            ESTABLISH UPLOAD RECORD                      
*                                                                               
         XC    WHDSAVE(WHDSAVEL),WHDSAVE CLEAR HEADER SAVE AREAS                
*                                                                               
*        VALIDATE MEDIA COMPANY AS USER ID                                      
*                                                                               
         XC    KEY,KEY             ESTABLISH KEY AS ID RECORD KEY               
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
*                                                                               
         MVI   CTIKTYP,CTIKTYPQ    SET RECORD ID                                
*        MVC   CTIKID,NDHDMDCO     SET USER ID IN KEY                           
         MVC   CTIKID,=CL10'TPNY'                                               
*                                                                               
         GOTOR DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,AIO1                      
         BNZ   PRHDUIDE            ERROR ID NOT FOUND                           
*                                                                               
*        FIND DESCRIPTION ELEMENT                                               
*                                                                               
         L     R4,AIO1             ESTABLISH ID RECORD                          
         USING CTIREC,R4                                                        
*                                                                               
         LA    R6,CTIDATA          POINT TO FIRST ELEMENT                       
*                                                                               
PRHDIDLP DS    0H                                                               
*                                                                               
         CLI   0(R6),0             ERROR AT END OF RECORD                       
         BE    PRHDUIDE                                                         
*                                                                               
         CLI   0(R6),CTDSCELQ      SEARCH FOR DESCRIPTION ELEMENT               
         BE    PRHDIDFD                                                         
*                                                                               
PTHDIDCN DS    0H                                                               
         LLC   RF,1(R6)            ELEMENT LENGTH                               
         LA    R6,0(RF,R6)         NEXT ELEMENT                                 
         B     PRHDIDLP                                                         
*                                                                               
PRHDIDFD DS    0H                                                               
*                                                                               
         USING CTDSCD,R6           ESTABLISH DESCRIPTION ELEMENT                
*                                                                               
         MVC   WHDUID,CTDSC        SAVE USERID NUMBER                           
*                                                                               
         MVC   WHDMDCO,NDHDMDCO    SAVE MEDIA COMPANY CODE                      
*                                                                               
         GOTOR DTEVAL,PARMS,NDHDDATE,WHDDATEP VALIDATE AND SAVE DATE            
         BNZ   PRHDDTEE               ERROR                                     
*                                                                               
         MVC   WHDDATE,NDHDDATE    SAVE HEADER DATE                             
*                                                                               
         GOTOR TMEVAL,PARMS,NDHDTIME,WHDTIMEP VALIDATE AND SAVE TIME            
         BNZ   PRHDTMEE               ERROR                                     
*                                                                               
         MVC   WHDTIME,NDHDTIME    SAVE HEADER TIME                             
*                                                                               
         CLC   NDHDFLNM,SPACES     MUST HAVE A FILE NAME                        
         BNH   PRHDFILE                                                         
*                                                                               
         MVC   WHDFLNM,NDHDFLNM    SAVE HEADER FILE NAME                        
*                                                                               
         MVC   WHDMDNM,NDHDMDNM    SAVE MEDIA COMPANY NAME                      
*                                                                               
*        CHECK IF BATCH ALREADY PROCESSED                                       
*                                                                               
         XC    WFLKYSV,WFLKYSV     INIT LATEST KEY                              
*                                                                               
         XC    KEY,KEY             ESTABLISH KEY FOR SYSTEM RECORD              
         USING TLSYKEY,R4                                                       
         LA    R4,KEY                                                           
*                                                                               
         MVI   TLSYCD,TLSYCDQ      SET RECORD CODE                              
         MVI   TLSYTYPE,TLSYNCFN   SET TYPE AS NON-CLARUS FILENAMES             
         MVC   TLSYNCDT,WHDDATEP   SET FILE DATE                                
         MVC   TLSYNUID,WHDUID     SET USERID                                   
                                                                                
         GOTOR HIGH                READ FOR RECORD                              
*                                                                               
PRHDRCLP DS    0H                                                               
*                                                                               
         CLC   TLSYKEY(TLSYSEQ-TLSYKEY),KEYSAVE DONE IF NO MATCH                
         BNE   PRHDRCDN                                                         
*                                                                               
         MVC   WFLKYSV,KEY         SAVE LATEST KEY                              
*                                                                               
*        FIND FILENAME ELEMENT                                                  
*                                                                               
         MVC   AIO,AIO2            READ RECORD INTO AIO2                        
*                                                                               
         GOTOR GETREC                                                           
*                                                                               
         MVC   AIO,AIO1            RESTORE IOAREA ADDRESS                       
*                                                                               
         L     R4,AIO2             ESTABLISH SYSTEM RECORD                      
*                                                                               
         L     R6,AIO2             START OF RECORD                              
         MVI   ELCODE,TAFIELQ      WANT COMMENT ELEMENT                         
         BRAS  RE,GETEL            FIND FIRST ELEMENT                           
*                                                                               
PRHDFLLP DS    0H                                                               
*                                                                               
         BNE   PRHDFLDN            NO ELEMENTS                                  
*                                                                               
         USING TAFID,R6            ESTABLISH ELEMENT                            
*                                                                               
         CLC   TAFIDATE,WHDDATE    MATCH ON DATE                                
         BNE   PRHDFLCN                                                         
*                                                                               
         CLC   TAFITIME,WHDTIME    MATCH ON TIME                                
         BNE   PRHDFLCN                                                         
*                                                                               
         LLC   RF,TAFILEN          GET ELEMENT LENGTH                           
         SHI   RF,TAFILNQ          DECREMENT BY HEADER LENGTH                   
         BNP   PRHDFLCN            NO FILENAME AVAILABLE                        
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TAFIFILE(0),WHDFLNM  MATCH ON FILE NAME                          
         BE    PRHDFLFD                                                         
*                                                                               
PRHDFLCN DS    0H                                                               
         BRAS  RE,NEXTEL           FIND NEXT COMMENT ELEMENT                    
         B     PRHDFLLP                                                         
*                                                                               
PRHDFLFD DS    0H                                                               
*                                                                               
         CLI   WRERUNOP,C'Y'       IF RERUN OPTION ON                           
         BNE   *+12                                                             
         MVI   WBYPASS,C'Y'           SET BYPASS SWITCH                         
         B     PRHDOKAY               DONE                                      
*                                                                               
PRHDFLF1 DS    0H                                                               
*                                                                               
         B     PRHDDUPE            ERROR - DUPLICATE FILENAMES                  
*                                                                               
PRHDFLDN DS    0H                                                               
*                                                                               
PRHDRCCN DS    0H                  FIND NEXT RECORD                             
         LA    R4,KEY              RE-POINT TO KEY AREA                         
         GOTOR SEQ                                                              
*                                                                               
         B     PRHDRCLP                                                         
*                                                                               
PRHDRCDN DS    0H                  NO PRIOR UPLOAD - PROCESS                    
*                                                                               
*        INITIALIZE SORTER                                                      
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
         MVI   WSORTSW,X'FF'       INDICATE SORT INITIALIZED                    
*                                                                               
PRHDOKAY DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     PRHDX                                                            
*                                                                               
*        ERROR MESSAGES                                                         
*                                                                               
PRHDUIDE DS    0H                  INVALID USERID                               
         LAY   R2,ERRUID           SET ERROR MESSAGE                            
*                                                                               
         B     PRHDER                                                           
*                                                                               
PRHDDTEE DS    0H                  INVALID DATE                                 
         LAY   R2,ERRDTE           SET ERROR MESSAGE                            
*                                                                               
         B     PRHDER                                                           
*                                                                               
PRHDTMEE DS    0H                  INVALID TIME                                 
         LAY   R2,ERRTIME          SET ERROR MESSAGE                            
*                                                                               
         B     PRHDER                                                           
*                                                                               
PRHDTRLE DS    0H                  MISSING TRAILER RECORD                       
         LAY   R2,ERRTRLE          SET ERROR MESSAGE                            
*                                                                               
         B     PRHDER                                                           
*                                                                               
PRHDFILE DS    0H                  MISSING FILE NAME                            
         LAY   R2,ERRFILE          SET ERROR MESSAGE                            
*                                                                               
         B     PRHDER                                                           
*                                                                               
PRHDDUPE DS    0H                  DUPLICATE FILENAMES                          
         LAY   R2,ERRDUPE          SET ERROR MESSAGE                            
*                                                                               
         B     PRHDER                                                           
*                                                                               
PRHDER   DS    0H                  ERROR EXIT                                   
*                                                                               
         MVC   WHDMDCO,NDHDMDCO    SAVE MEDIA COMPANY CODE                      
         MVC   WHDDATE,NDHDDATE    SAVE HEADER DATE                             
         MVC   WHDTIME,NDHDTIME    SAVE HEADER DATE                             
         MVC   WHDFLNM,NDHDFLNM    SAVE HEADER FILE NAME                        
         MVC   WHDMDNM,NDHDMDNM    SAVE MEDIA COMPANY NAME                      
*                                                                               
         GOTOR SMTPERR,PARMS,(0,0(R2)),(0,0(R3))  E-MAIL ERROR                  
*                                                                               
         LTR   RB,RB               SET NEQ CC                                   
*                                                                               
PRHDX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,32,A),FORMAT=BI '                            
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=296'                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - PRTW'                                 
***********************************************************************         
*                                                                     *         
*        TV WILD SPOTS                                                *         
*                                                                     *         
*NTRY    R3==> UPLOAD RECORD                                          *         
*                                                                     *         
***********************************************************************         
                                                                                
PRTW     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   WBYPASS,C'Y'        SKIP IF BYPASSING BATCH                      
         BE    PRTWOKAY                                                         
*                                                                               
         USING NDTWD,R3            ESTABLISH UPLOAD RECORD                      
*                                                                               
         XC    SRTKEY(SRTHDRLN),SRTKEY  INIT SORT RECORD                        
         XC    SRTDATA,SRTDATA     INIT SORT RECORD                             
*                                                                               
         MVC   SRTDATA,0(R3)       COPY RECORD TO SORT RECORD                   
*                                                                               
         MVI   SRTTYPE,C'S'        SPOT RECORD TYPE                             
*                                                                               
         CLC   NDTWUPID,SPACES     UNIQUE ID REQUIRED                           
         BNH   PRTWAGYI                                                         
*                                                                               
*                                                                               
*        VERIFY AGENCY CODE IS ON FILE                                          
*                                                                               
         GOTOR AGYVAL,DMCB,NDTWAGY,WAGY  VALIDATE AGENCY                        
         BNE   PRTWAGYE               DROP IF AGENCY NOT VALID                  
*                                                                               
         MVC   SRTAGY,WAGY         SET AGENCY IN SORT KEY                       
*                                                                               
         CLC   NDTWCMID,SPACES     COMMERICAL ID REQUIRED                       
         BNH   PRTWERR3                                                         
*                                                                               
         MVC   SRTCMID,NDTWCMID    SET COMMERCIAL ID                            
*                                                                               
         MVC   DUB(3),NDTWCMLN     VALIDATE COMM LENGTH AS NUMERIC              
         NC    DUB(3),=8C'0'                                                    
         CLC   DUB(3),=8C'0'                                                    
         BNE   PRTWERR4               NOT NUMERIC                               
*                                                                               
         PACK  DUB,NDTWCMLN        CVD                                          
         CVB   RF,DUB              CVB                                          
         STC   RF,SRTCMLN          PASS LENGTH IN SORT RECORD                   
*                                                                               
         CLC   NDTWCMTL,SPACES     COMML TITLE REQUIRED                         
         BNH   PRTWERR5                                                         
         CLC   NDTWCLCD,SPACES     CLIENT CODE REQUIRED                         
         BNH   PRTWERR6                                                         
         CLC   NDTWCLNM,SPACES     CLIENT NAME REQUIRED                         
         BNH   PRTWERR7                                                         
         CLC   NDTWPRCD,SPACES     PRODUCT CODE REQUIRED                        
         BNH   PRTWERR8                                                         
         CLC   NDTWPRNM,SPACES     PRODUCT NAME REQUIRED                        
         BNH   PRTWERR9                                                         
         CLC   NDTWMKCD,SPACES     TALENT MARKET REQUIRED                       
         BNH   PRTWERRA                                                         
         CLC   NDTWMKNM,SPACES     MARKET NAME REQUIRED                         
         BNH   PRTWERRB                                                         
*                                                                               
         MVI   BYTE,C'T'                                                        
         MVC   DUB(8),DUB                                                       
         MVC   DUB(4),NDTWMKCD                                                  
         BRAS  RE,VALMCS           VALIDATE MARKET/CABLE/SYSTEM                 
*        BNE   PRTWERRA            INVALID TALENT MARKET                        
*                                                                               
         MVC   SRTCLCD,NDTWCLCD    SET CLIENT CODE                              
         MVC   SRTPRCD,NDTWPRCD    SET PRODUCT CODE                             
         MVI   SRTMDCD,C'T'        MEDIA IS TELEVISION                          
*                                                                               
         MVC   SRTRECID,NDTWID     SET RECORD ID                                
*                                                                               
         GOTOR DTEVAL,PARMS,NDTWASTR,SRTDATE  SET DATE                          
         BNZ   PRTWERR1               DATE IN ERROR                             
*                                                                               
         MVC   SRTFLNM,WHDFLNM     SET UNIQUE FILE NAME                         
*                                                                               
         BRAS  RE,PUTSORT          ADD TO SORT                                  
*                                                                               
         GOTOR DTEVAL,PARMS,NDTWAEND,SRTDATE  SET END DATE                      
         BNZ   PRTWERR2               DATE IN ERROR                             
*                                                                               
         BRAS  RE,PUTSORT          ADD TO SORT                                  
*                                                                               
PRTWOKAY DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     PRTWX                                                            
*                                                                               
PRTWAGYI DS    0H                  INVALID ID CODE                              
         LAY   R2,ERRINVID                                                      
         B     PRTWERR                                                          
*                                                                               
PRTWAGYE DS    0H                  INVALID AGENCY CODE                          
         LAY   R2,ERRAGY                                                        
         B     PRTWERR                                                          
*                                                                               
PRTWERR1 DS    0H                  INVALID START DATE                           
         LAY   R2,ERRSTRDT                                                      
         B     PRTWERR                                                          
*                                                                               
PRTWERR2 DS    0H                  INVALID END DATE                             
         LAY   R2,ERRENDDT                                                      
         B     PRTWERR                                                          
*                                                                               
PRTWERR3 DS    0H                  INVALID COMMERCIAL                           
         LAY   R2,ERRCOM                                                        
         B     PRTWERR                                                          
*                                                                               
PRTWERR4 DS    0H                  INVALID COMMERCIAL LENGTH                    
         LAY   R2,ERRCOMLN                                                      
         B     PRTWERR                                                          
*                                                                               
PRTWERR5 DS    0H                  INVALID COMMERCIAL TITLE                     
         LAY   R2,ERRCOMTI                                                      
         B     PRTWERR                                                          
*                                                                               
PRTWERR6 DS    0H                  INVALID CLIENT CODE                          
         LAY   R2,ERRCLI                                                        
         B     PRTWERR                                                          
*                                                                               
PRTWERR7 DS    0H                  INVALID CLIENT NAME                          
         LAY   R2,ERRCLINM                                                      
         B     PRTWERR                                                          
*                                                                               
PRTWERR8 DS    0H                  INVALID PRODUCT CODE                         
         LAY   R2,ERRPRD                                                        
         B     PRTWERR                                                          
*                                                                               
PRTWERR9 DS    0H                  INVALID PRODUCT NAME                         
         LAY   R2,ERRPRDNM                                                      
         B     PRTWERR                                                          
*                                                                               
PRTWERRA DS    0H                  INVALID TALENT MARKET                        
         LAY   R2,ERRTMKT                                                       
         B     PRTWERR                                                          
*                                                                               
PRTWERRB DS    0H                  INVALID TALENT MARKET NAME                   
         LAY   R2,ERRTMKTN                                                      
         B     PRTWERR                                                          
*                                                                               
PRTWERR  DS    0H                  ERROR SKIP RECORD                            
*                                                                               
         GOTOR SMTPERR,PARMS,(0,0(R2)),(0,0(R3))  E-MAIL ERROR                  
*                                                                               
         LTR   RB,RB               SET NEQ CC                                   
*                                                                               
PRTWX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - SRTW'                                 
***********************************************************************         
*                                                                     *         
*        TV WILD SPOTS                                                *         
*                                                                     *         
*NTRY    R2    - TV WILD SPOT SORTED RECORD                           *         
*                                                                     *         
***********************************************************************         
                                                                                
SRTW     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,SRTDATA          ESTABLISH TV WILD SPOT DATA                  
         USING NDTWD,R3            ESTABLISH UPLOAD RECORD                      
*                                                                               
*        ADD TRANSFER ELEMENT TO RECORD                                         
*                                                                               
SRTWNX   DS    0H                                                               
*                                                                               
         BRAS  RE,INITWRK          INIT WORKER FILE AREA                        
         LA    R6,MYWORK           SET UP AS TRANSFER ELEMENT                   
*                                                                               
         USING TANXD,R6            ESTABLISH TRANSFER ELEMENT                   
*                                                                               
         XC    TANXEL(TANXLN2Q+1),TANXEL   INIT ELEMENT                         
*                                                                               
         MVI   TANXEL,TANXELQ      SET ELEMENT ID                               
         MVI   TANXLEN,TANXLNQ     SET ELEMENT LENGTH                           
         MVC   TANXAGY,SRTAGY      SET AGENCY CODE                              
         MVC   TANXUID,WHDMDCO     SET USER ID                                  
*                                                                               
         MVC   TANXNCID,NDTWCMID   SET COMMERCIAL ID                            
*                                                                               
         CLC   NDTWCMID+8(4),SPACES SKIP IF 8 CH CID                            
         BNH   SRTW10                                                           
*                                                                               
         GOTO1 ADTRPACK,DMCB,(C'P',NDTWCMID),TANXNCID PACK CID                  
         OI    TANXSTAT,TANXPACK   PACKED CID                                   
         MVI   TANXLEN,TANXLN2Q                                                 
         MVC   TANXADID,NDTWCMID                                                
*                                                                               
SRTW10   DS    0H                                                               
*                                                                               
         MVC   TANXSEC,SRTCMLN     SET COMMERCIAL LENGTH                        
         MVC   TANXUDTE,SRTDATE    SET DATE USED                                
         MVC   TANXADTE,WTODAY     SET DATE ADDED                               
         OI    TANXSTAT,TANXNDDS   NON-DDS CLARUS                               
         MVC   TANXMED,SRTMDCD     SET MEDIA                                    
*                                                                               
         DROP  R6                                                               
*                                                                               
SRTWNXX  DS    0H                                                               
*                                                                               
*        ADD   CLIENT CODE ELEMENT                                              
*                                                                               
SRTWCT   DS    0H                                                               
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TACTD,R6            ESTABLISH CLIENT CODE ELEMENT                
*                                                                               
         MVI   TACTEL,TACTELQ      CLIENT ELEMENT                               
         MVI   TACTLEN,TACTLNQ     ELEMENT LENGTH                               
         MVC   TACTCLI,NDTWCLCD    SET CLIENT CODE                              
         OC    TACTCLI,SPACES      FORCE UPPERCASE                              
*                                                                               
         DROP  R6                                                               
*                                                                               
SRTWCTX  DS    0H                                                               
*                                                                               
*        ADD   PRODUCT CODE ELEMENT                                             
*                                                                               
SRTWPR   DS    0H                                                               
*                                                                               
         CLC   NDTWPRCD,SPACES     IF UNALLOCATED                               
         BE    SRTWPRX             SKIP PROD CODE ELEMENT                       
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAPRD,R6            ESTABLISH AS PRODUCT CODE ELEMENT            
*                                                                               
         MVI   TAPREL,TAPRELQ      PRODUCT CODE ELEMENT                         
         MVI   TAPRLEN,TAPRLNQ     ELEMENT LENGTH                               
         MVC   TAPRPRD,NDTWPRCD    SET PROD CODE                                
         OC    TAPRPRD,SPACES      FORCE UPPERCASE                              
*                                                                               
         DROP  R6                                                               
*                                                                               
SRTWPRX  DS    0H                                                               
*                                                                               
*        ADD CLIENT NAME ELEMENT                                                
*                                                                               
SRTWCFN  DS    0H                                                               
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAFND,R6            ESTABLISH AS CLIENT NAME ELEMENT             
*                                                                               
         MVI   TAFNEL,TAFNELQ      SET ELEMENT ID                               
         MVI   TAFNTYPE,TAFNTCLI   SET AS CLIENT NAME ELM                       
         GOTO1 SQUASHER,DMCB,NDTWCLNM,L'NDTWCLNM                                
         L     RF,DMCB+4           GET LENGTH OF TITLE                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),NDTWCLNM SET CLIENT NAME                             
         AHI   RF,4                ADD FOR CORRECT ELEMENT LENGTH               
         STC   RF,TAFNLEN                                                       
         DROP  R6                                                               
*                                                                               
*        ADD PRODUCT NAME ELEMENT                                               
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAFND,R6            ESTABLISH AS PRODUCT NAME ELEMENT            
*                                                                               
         MVI   TAFNEL,TAFNELQ      SET ELEMENT ID                               
         MVI   TAFNTYPE,TAFNTPRD   SET AS PRODUCT NAME ELM                      
         GOTO1 SQUASHER,DMCB,NDTWPRNM,L'NDTWPRNM                                
         L     RF,DMCB+4           GET LENGTH OF TITLE                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),NDTWPRNM SET PRODUCT NAME                            
         AHI   RF,4                ADD FOR CORRECT ELEMENT LENGTH               
         STC   RF,TAFNLEN                                                       
         DROP  R6                                                               
*                                                                               
*        ADD COMMERCIAL TITLE ELEMENT                                           
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAFND,R6            ESTABLISH AS NAME ELEMENT                    
*                                                                               
         MVI   TAFNEL,TAFNELQ      SET ELEMENT ID                               
         MVI   TAFNTYPE,TAFNTTTL   SET AS COMMERCIAL TITLE ELM                  
         GOTO1 SQUASHER,DMCB,NDTWCMTL,L'NDTWCMTL                                
         L     RF,DMCB+4           GET LENGTH OF TITLE                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),NDTWCMTL SET COMMERCIAL TITLE                        
         AHI   RF,4                ADD FOR CORRECT ELEMENT LENGTH               
         STC   RF,TAFNLEN                                                       
         DROP  R6                                                               
*                                                                               
*        ADD PROGRAM DETAILS/MARKET ELEMENT TO RECORD                           
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TANPD,R6            ESTABLISH AS PROGRAM ELEMENT                 
*                                                                               
         MVI   TANPD,TANPELQ       SET ELEMENT TYPE                             
         MVI   TANPLEN,TANPLNQ4    SET ELEMENT LENGTH                           
         MVI   TANPSEQ,0           SET SEQUENCE NUMBER                          
         MVC   TANPDATE,SRTDATE    SET USE DATE                                 
         MVI   TANPTYP,TANPSPT     ELEMENT TYPE                                 
*                                                                               
         LA    R1,4                                                             
         LA    RE,NDTWMKCD                                                      
SRTWM10  CLI   0(RE),C'A'                                                       
         BNL   *+8                                                              
         MVI   0(RE),0             GET RID OF SPACES                            
         AHI   RE,1                                                             
         BCT   R1,SRTWM10                                                       
         MVC   TANPMKT,NDTWMKCD    SET MARKET CODE                              
         MVC   TANPMKTN,NDTWMKNM   SET MARKET NAME                              
*                                                                               
*        ADD MEDIA COMPANY NAME                                                 
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         GOTOR MEDCPYNM                                                         
*                                                                               
*        ADD UNIQUE FILE NAME ELEMENT                                           
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*&&DO                                                                           
         USING TACMD,R6            ESTABLISH AS COMMENT ELEMENT                 
*                                                                               
         MVI   TACMEL,TACMELQ      SET ELEMENT ID                               
         MVI   TACMTYPE,TACMTYPF   SET AS FILENAME COMMENT                      
         MVC   TACMCOMM(L'WHDFLNM),WHDFLNM SET FILW NAME                        
*                                                                               
         LA    RF,TACMCOMM+L'WHDFLNM-1 END OF FILE NAME                         
*                                                                               
         CLI   0(RF),C' '          FIND LAST CHARACTER OF NAME                  
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         LA    RF,1(RF)            POINT TO NEXT POSITION                       
         SR    RF,R6               LENGTH OF ELEMENT                            
         STC   RF,TACMLEN          SET ELEMENT LENGTH                           
*&&                                                                             
*                                                                               
         GOTOR MOVEWRKR            MOVE ELMS TO WORKER IO AREA                  
*                                                                               
         GOTOR ADDWRKR             ADD DATA TO WORKER FILE                      
*                                                                               
SRTWUPDX DS    0H                                                               
*                                                                               
*        PRINT DETAIL LINE                                                      
*                                                                               
         CLI   TRACOPT,C'Y'        SKIP IF TRACING                              
         BE    SRTWOK                                                           
*                                                                               
         CLI   REPTOPT,C'Y'        SKIP IF NOT PRINTING REPORT                  
         BNE   SRTWOK                                                           
*                                                                               
         MVI   RCSUBPRG,28         DEFAULT PAGE TYPE                            
*                                                                               
         MVC   P,SPACES                                                         
         LA    R2,P                ESTABLISH PRINT LINE                         
         BRAS  RE,CNTR             CENTER REPORT                                
         USING PRWD,R2                                                          
*                                                                               
         MVC   PRWNID,SRTCMID      COMMERCIAL ID                                
         MVC   PRWTTL,NDTWCMTL     COMMERCIAL TITLE                             
         MVC   PRWNLEN,NDTWCMLN    SPOT LENGTH                                  
         MVC   PRWUSED,NDTWASTR    ACTIVE START DATE                            
         MVC   PRWMKTCD,NDTWMKCD   MARKET CODE                                  
         MVC   PRWMKTNM,NDTWMKNM   MARKET NAME                                  
*                                                                               
         BRAS  RE,SPLAT                                                         
*                                                                               
SRTWOK   DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     SRTWX                                                            
*                                                                               
SRTWERR  DS    0H                  ERROR SKIP RECORD                            
*                                                                               
SRTWERR1 DS    0H                  ERROR SKIP RECORD                            
*                                                                               
         LTR   RB,RB               SET NEQ CC                                   
*                                                                               
SRTWX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - PRRW'                                 
***********************************************************************         
*                                                                     *         
*        RADIO WILD SPOTS                                             *         
*                                                                     *         
*NTRY    R3==> UPLOAD RECORD                                          *         
*                                                                     *         
***********************************************************************         
                                                                                
PRRW     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   WBYPASS,C'Y'        SKIP IF BYPASSING BATCH                      
         BE    PRRWOKAY                                                         
*                                                                               
         USING NDRWD,R3            ESTABLISH UPLOAD RECORD                      
*                                                                               
         XC    SRTKEY(SRTHDRLN),SRTKEY  INIT SORT RECORD                        
         XC    SRTDATA,SRTDATA     INIT SORT RECORD                             
*                                                                               
         MVC   SRTDATA,0(R3)       COPY RECORD TO SORT RECORD                   
*                                                                               
         MVI   SRTTYPE,C'S'        SPOT RECORD TYPE                             
*                                                                               
         CLC   NDRWUPID,SPACES     UNIQUE ID REQUIRED                           
         BNH   PRRWAGYI                                                         
*                                                                               
*        VERIFY AGENCY CODE IS ON FILE                                          
*                                                                               
         GOTOR AGYVAL,DMCB,NDRWAGY,WAGY  VALIDATE AGENCY                        
         BNE   PRRWAGYE               DROP IF AGENCY NOT VALID                  
*                                                                               
         MVC   SRTAGY,WAGY         SET AGENCY IN SORT KEY                       
*                                                                               
         CLC   NDRWCMID,SPACES     COMMERICAL ID REQUIRED                       
         BNH   PRRWERR3                                                         
*                                                                               
         MVC   SRTCMID,NDRWCMID    SET COMMERCIAL ID                            
*                                                                               
         MVC   DUB(3),NDRWCMLN     VALIDATE COMM LENGTH AS NUMERIC              
         NC    DUB(3),=8C'0'                                                    
         CLC   DUB(3),=8C'0'                                                    
         BNE   PRRWERR4               NOT NUMERIC                               
*                                                                               
         PACK  DUB,NDRWCMLN        CVD                                          
         CVB   RF,DUB              CVB                                          
         STC   RF,SRTCMLN          PASS LENGTH IN SORT RECORD                   
*                                                                               
         CLC   NDRWCMTL,SPACES     COMML TITLE REQUIRED                         
         BNH   PRRWERR5                                                         
         CLC   NDRWCLCD,SPACES     CLIENT CODE REQUIRED                         
         BNH   PRRWERR6                                                         
         CLC   NDRWCLNM,SPACES     CLIENT NAME REQUIRED                         
         BNH   PRRWERR7                                                         
         CLC   NDRWPRCD,SPACES     PRODUCT CODE REQUIRED                        
         BNH   PRRWERR8                                                         
         CLC   NDRWPRNM,SPACES     PRODUCT NAME REQUIRED                        
         BNH   PRRWERR9                                                         
         CLC   NDRWMKCD,SPACES     TALENT MARKET REQUIRED                       
         BNH   PRRWERRA                                                         
         CLC   NDRWMKNM,SPACES     MARKET NAME REQUIRED                         
         BNH   PRRWERRB                                                         
*                                                                               
         MVI   BYTE,C'R'                                                        
         MVC   DUB(8),DUB                                                       
         MVC   DUB(4),NDRWMKCD                                                  
         BRAS  RE,VALMCS           VALIDATE MARKET/CABLE/SYSTEM                 
*        BNE   PRRWERRA            INVALID TALENT MARKET                        
*                                                                               
         MVC   SRTFLNM,WHDFLNM     SET UNIQUE FILE NAME                         
*                                                                               
         MVC   SRTCLCD,NDRWCLCD    SET CLIENT CODE                              
         MVC   SRTPRCD,NDRWPRCD    SET PRODUCT CODE                             
         MVI   SRTMDCD,C'R'        MEDIA IS RADIO                               
*                                                                               
         MVC   SRTRECID,NDRWID     SET RECORD ID                                
*                                                                               
         GOTOR DTEVAL,PARMS,NDRWASTR,SRTDATE  SET DATE                          
         BNZ   PRRWERR1               DATE IN ERROR                             
*                                                                               
         BRAS  RE,PUTSORT          ADD TO SORT                                  
*                                                                               
         GOTOR DTEVAL,PARMS,NDRWAEND,SRTDATE  SET END DATE                      
         BNZ   PRRWERR2               DATE IN ERROR                             
*                                                                               
         BRAS  RE,PUTSORT          ADD TO SORT                                  
*                                                                               
PRRWOKAY DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     PRRWX                                                            
*                                                                               
PRRWAGYI DS    0H                  INVALID ID CODE                              
         LAY   R2,ERRINVID                                                      
         B     PRRWERR                                                          
*                                                                               
PRRWAGYE DS    0H                  INVALID AGENCY CODE                          
         LAY   R2,ERRAGY                                                        
         B     PRRWERR                                                          
*                                                                               
PRRWERR1 DS    0H                  INVALID START DATE                           
         LAY   R2,ERRSTRDT                                                      
         B     PRRWERR                                                          
*                                                                               
PRRWERR2 DS    0H                  INVALID END DATE                             
         LAY   R2,ERRENDDT                                                      
         B     PRRWERR                                                          
*                                                                               
PRRWERR3 DS    0H                  INVALID COMMERCIAL                           
         LAY   R2,ERRCOM                                                        
         B     PRRWERR                                                          
*                                                                               
PRRWERR4 DS    0H                  INVALID COMMERCIAL LENGTH                    
         LAY   R2,ERRCOMLN                                                      
         B     PRRWERR                                                          
*                                                                               
PRRWERR5 DS    0H                  INVALID COMMERCIAL TITLE                     
         LAY   R2,ERRCOMTI                                                      
         B     PRRWERR                                                          
*                                                                               
PRRWERR6 DS    0H                  INVALID CLIENT CODE                          
         LAY   R2,ERRCLI                                                        
         B     PRRWERR                                                          
*                                                                               
PRRWERR7 DS    0H                  INVALID CLIENT NAME                          
         LAY   R2,ERRCLINM                                                      
         B     PRRWERR                                                          
*                                                                               
PRRWERR8 DS    0H                  INVALID PRODUCT CODE                         
         LAY   R2,ERRPRD                                                        
         B     PRRWERR                                                          
*                                                                               
PRRWERR9 DS    0H                  INVALID PRODUCT NAME                         
         LAY   R2,ERRPRDNM                                                      
         B     PRRWERR                                                          
*                                                                               
PRRWERRA DS    0H                  INVALID TALENT MARKET                        
         LAY   R2,ERRTMKT                                                       
         B     PRRWERR                                                          
*                                                                               
PRRWERRB DS    0H                  INVALID TALENT MARKET NAME                   
         LAY   R2,ERRTMKTN                                                      
         B     PRRWERR                                                          
*                                                                               
PRRWERR  DS    0H                  ERROR SKIP RECORD                            
*                                                                               
         GOTOR SMTPERR,PARMS,(0,0(R2)),(0,0(R3))  E-MAIL ERROR                  
*                                                                               
         LTR   RB,RB               SET NEQ CC                                   
*                                                                               
PRRWX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - SRRW'                                 
***********************************************************************         
*                                                                     *         
*        TV WILD SPOTS                                                *         
*                                                                     *         
*NTRY    R2    - TV WILD SPOT SORTED RECORD                           *         
*                                                                     *         
***********************************************************************         
                                                                                
SRRW     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,SRTDATA          ESTABLISH TV WILD SPOT DATA                  
         USING NDRWD,R3            ESTABLISH UPLOAD RECORD                      
*                                                                               
*        ADD TRANSFER ELEMENT TO RECORD                                         
*                                                                               
SRRWNX   DS    0H                                                               
*                                                                               
         BRAS  RE,INITWRK          INIT WORKER FILE AREA                        
         LA    R6,MYWORK           SET UP AS TRANSFER ELEMENT                   
*                                                                               
         USING TANXD,R6            ESTABLISH TRANSFER ELEMENT                   
*                                                                               
         XC    TANXEL(TANXLN2Q+1),TANXEL   INIT ELEMENT                         
*                                                                               
         MVI   TANXEL,TANXELQ      SET ELEMENT ID                               
         MVI   TANXLEN,TANXLNQ     SET ELEMENT LENGTH                           
         MVC   TANXAGY,SRTAGY      SET AGENCY CODE                              
         MVC   TANXUID,WHDMDCO     SET USER ID                                  
*                                                                               
         MVC   TANXNCID,NDRWCMID   SET COMMERCIAL ID                            
*                                                                               
         CLC   NDRWCMID+8(4),SPACES SKIP IF 8 CH CID                            
         BNH   SRRW10                                                           
*                                                                               
         GOTO1 ADTRPACK,DMCB,(C'P',NDRWCMID),TANXNCID PACK CID                  
         OI    TANXSTAT,TANXPACK   PACKED CID                                   
         MVI   TANXLEN,TANXLN2Q                                                 
         MVC   TANXADID,NDRWCMID                                                
*                                                                               
SRRW10   DS    0H                                                               
*                                                                               
         MVC   TANXSEC,SRTCMLN     SET COMMERCIAL LENGTH                        
         MVC   TANXUDTE,SRTDATE    SET DATE USED                                
         MVC   TANXADTE,WTODAY     SET DATE ADDED                               
         OI    TANXSTAT,TANXNDDS   NON-DDS CLARUS                               
         MVC   TANXMED,SRTMDCD     SET MEDIA                                    
*                                                                               
         DROP  R6                                                               
*                                                                               
SRRWNXX  DS    0H                                                               
*                                                                               
*        ADD   CLIENT CODE ELEMENT                                              
*                                                                               
SRRWCT   DS    0H                                                               
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TACTD,R6            ESTABLISH CLIENT CODE ELEMENT                
*                                                                               
         MVI   TACTEL,TACTELQ      CLIENT ELEMENT                               
         MVI   TACTLEN,TACTLNQ     ELEMENT LENGTH                               
         MVC   TACTCLI,NDRWCLCD    SET CLIENT CODE                              
         OC    TACTCLI,SPACES      FORCE UPPERCASE                              
*                                                                               
         DROP  R6                                                               
*                                                                               
SRRWCTX  DS    0H                                                               
*                                                                               
*        ADD   PRODUCT CODE ELEMENT                                             
*                                                                               
SRRWPR   DS    0H                                                               
*                                                                               
         CLC   NDRWPRCD,SPACES     IF UNALLOCATED                               
         BE    SRRWPRX             SKIP PROD CODE ELEMENT                       
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAPRD,R6            ESTABLISH AS PRODUCT CODE ELEMENT            
*                                                                               
         MVI   TAPREL,TAPRELQ      PRODUCT CODE ELEMENT                         
         MVI   TAPRLEN,TAPRLNQ     ELEMENT LENGTH                               
         MVC   TAPRPRD,NDRWPRCD    SET PROD CODE                                
         OC    TAPRPRD,SPACES      FORCE UPPERCASE                              
*                                                                               
         DROP  R6                                                               
*                                                                               
SRRWPRX  DS    0H                                                               
*                                                                               
*        ADD CLIENT NAME ELEMENT                                                
*                                                                               
SRRWCFN  DS    0H                                                               
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAFND,R6            ESTABLISH AS CLIENT NAME ELEMENT             
*                                                                               
         MVI   TAFNEL,TAFNELQ      SET ELEMENT ID                               
         MVI   TAFNTYPE,TAFNTCLI   SET AS CLIENT NAME ELM                       
         GOTO1 SQUASHER,DMCB,NDRWCLNM,L'NDRWCLNM                                
         L     RF,DMCB+4           GET LENGTH OF TITLE                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),NDRWCLNM SET CLIENT NAME                             
         AHI   RF,4                ADD FOR CORRECT ELEMENT LENGTH               
         STC   RF,TAFNLEN                                                       
*                                                                               
*        ADD PRODUCT NAME ELEMENT                                               
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAFND,R6            ESTABLISH AS PRODUCT NAME ELEMENT            
*                                                                               
         MVI   TAFNEL,TAFNELQ      SET ELEMENT ID                               
         MVI   TAFNTYPE,TAFNTPRD   SET AS PRODUCT NAME ELM                      
         GOTO1 SQUASHER,DMCB,NDRWPRNM,L'NDRWPRNM                                
         L     RF,DMCB+4           GET LENGTH OF TITLE                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),NDRWPRNM SET PRODUCT NAME                            
         AHI   RF,4                ADD FOR CORRECT ELEMENT LENGTH               
         STC   RF,TAFNLEN                                                       
         DROP  R6                                                               
*                                                                               
*        ADD COMMERCIAL TITLE ELEMENT                                           
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAFND,R6            ESTABLISH AS NAME ELEMENT                    
*                                                                               
         MVI   TAFNEL,TAFNELQ      SET ELEMENT ID                               
         MVI   TAFNTYPE,TAFNTTTL   SET AS COMMERCIAL TITLE ELM                  
         GOTO1 SQUASHER,DMCB,NDRWCMTL,L'NDRWCMTL                                
         L     RF,DMCB+4           GET LENGTH OF TITLE                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),NDRWCMTL SET COMMERCIAL TITLE                        
         AHI   RF,4                ADD FOR CORRECT ELEMENT LENGTH               
         STC   RF,TAFNLEN                                                       
         DROP  R6                                                               
*                                                                               
*        ADD PROGRAM DETAILS/MARKET ELEMENT TO RECORD                           
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TANPD,R6            ESTABLISH AS PROGRAM ELEMENT                 
*                                                                               
         MVI   TANPD,TANPELQ       SET ELEMENT TYPE                             
         MVI   TANPLEN,TANPLNQ4    SET ELEMENT LENGTH                           
         MVI   TANPSEQ,0           SET SEQUENCE NUMBER                          
         MVC   TANPDATE,SRTDATE    SET USE DATE                                 
         MVI   TANPTYP,TANPSPT     ELEMENT TYPE                                 
         MVC   TANPMKT,NDRWMKCD    SET MARKET CODE                              
         MVC   TANPMKTN,NDRWMKNM   SET MARKET NAME                              
*                                                                               
*        ADD MEDIA COMPANY NAME                                                 
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         GOTOR MEDCPYNM                                                         
*                                                                               
*        ADD UNIQUE FILE NAME ELEMENT                                           
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*&&DO                                                                           
         USING TACMD,R6            ESTABLISH AS COMMENT ELEMENT                 
*                                                                               
         MVI   TACMEL,TACMELQ      SET ELEMENT ID                               
         MVI   TACMTYPE,TACMTYPF   SET AS FILENAME COMMENT                      
         MVC   TACMCOMM(L'WHDFLNM),WHDFLNM SET FILW NAME                        
*                                                                               
         LA    RF,TACMCOMM+L'WHDFLNM-1 END OF FILE NAME                         
*                                                                               
         CLI   0(RF),C' '          FIND LAST CHARACTER OF NAME                  
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         LA    RF,1(RF)            POINT TO NEXT POSITION                       
         SR    RF,R6               LENGTH OF ELEMENT                            
         STC   RF,TACMLEN          SET ELEMENT LENGTH                           
*&&                                                                             
         GOTOR MOVEWRKR            MOVE ELMS TO WORKER IO AREA                  
*                                                                               
         GOTOR ADDWRKR             ADD DATA TO WORKER FILE                      
*                                                                               
SRRWUPDX DS    0H                                                               
*                                                                               
*        PRINT DETAIL LINE                                                      
*                                                                               
         CLI   TRACOPT,C'Y'        SKIP IF TRACING                              
         BE    SRRWOK                                                           
*                                                                               
         CLI   REPTOPT,C'Y'        SKIP IF NOT PRINTING REPORT                  
         BNE   SRRWOK                                                           
*                                                                               
         MVI   RCSUBPRG,28         DEFAULT PAGE TYPE                            
*                                                                               
         MVC   P,SPACES                                                         
         LA    R2,P                ESTABLISH PRINT LINE                         
         BRAS  RE,CNTR             CENTER REPORT                                
         USING PRWD,R2                                                          
*                                                                               
         MVC   PRWNID,SRTCMID      COMMERCIAL ID                                
         MVC   PRWTTL,NDRWCMTL     COMMERCIAL TITLE                             
         MVC   PRWNLEN,NDRWCMLN    SPOT LENGTH                                  
         MVC   PRWUSED,NDRWASTR    ACTIVE START DATE                            
         MVC   PRWMKTCD,NDRWMKCD   MARKET CODE                                  
         MVC   PRWMKTNM,NDRWMKNM   MARKET NAME                                  
*                                                                               
         BRAS  RE,SPLAT                                                         
*                                                                               
SRRWOK   DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     SRRWX                                                            
*                                                                               
SRRWERR  DS    0H                  ERROR SKIP RECORD                            
*                                                                               
SRRWERR1 DS    0H                  ERROR SKIP RECORD                            
*                                                                               
         LTR   RB,RB               SET NEQ CC                                   
*                                                                               
SRRWX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - PRLC'                                 
***********************************************************************         
*                                                                     *         
*        LOCAL CABLE                                                  *         
*                                                                     *         
*NTRY    R3==> UPLOAD RECORD                                          *         
*                                                                     *         
***********************************************************************         
                                                                                
PRLC     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   WBYPASS,C'Y'        SKIP IF BYPASSING BATCH                      
         BE    PRLCOKAY                                                         
*                                                                               
         USING NDLCD,R3            ESTABLISH UPLOAD RECORD                      
*                                                                               
         XC    SRTKEY(SRTHDRLN),SRTKEY  INIT SORT RECORD                        
         XC    SRTDATA,SRTDATA     INIT SORT RECORD                             
*                                                                               
         MVC   SRTDATA,0(R3)       COPY RECORD TO SORT RECORD                   
*                                                                               
         MVI   SRTTYPE,C'S'        SPOT RECORD TYPE                             
*                                                                               
         CLC   NDLCUPID,SPACES     UNIQUE ID REQUIRED                           
         BNH   PRLCAGYI                                                         
*                                                                               
*        VERIFY AGENCY CODE IS ON FILE                                          
*                                                                               
         GOTOR AGYVAL,DMCB,NDLCAGY,WAGY  VALIDATE AGENCY                        
         BNE   PRLCAGYE               DROP IF AGENCY NOT VALID                  
*                                                                               
         MVC   SRTAGY,WAGY         SET AGENCY IN SORT KEY                       
*                                                                               
         CLC   NDLCCMID,SPACES     COMMERICAL ID REQUIRED                       
         BNH   PRLCERR3                                                         
*                                                                               
         MVC   SRTCMID,NDLCCMID    SET COMMERCIAL ID                            
*                                                                               
         MVC   DUB(3),NDLCCMLN     VALIDATE COMM LENGTH AS NUMERIC              
         NC    DUB(3),=8C'0'                                                    
         CLC   DUB(3),=8C'0'                                                    
         BNE   PRLCERR4               NOT NUMERIC                               
*                                                                               
         PACK  DUB,NDLCCMLN        CVD                                          
         CVB   RF,DUB              CVB                                          
         STC   RF,SRTCMLN          PASS LENGTH IN SORT RECORD                   
*                                                                               
         CLC   NDLCCMTL,SPACES     COMML TITLE REQUIRED                         
         BNH   PRLCERR5                                                         
         CLC   NDLCCLCD,SPACES     CLIENT CODE REQUIRED                         
         BNH   PRLCERR6                                                         
         CLC   NDLCCLNM,SPACES     CLIENT NAME REQUIRED                         
         BNH   PRLCERR7                                                         
         CLC   NDLCPRCD,SPACES     PRODUCT CODE REQUIRED                        
         BNH   PRLCERR8                                                         
         CLC   NDLCPRNM,SPACES     PRODUCT NAME REQUIRED                        
         BNH   PRLCERR9                                                         
         CLC   NDLCSYCD,SPACES     SYSTEM CODE REQUIRED                         
         BNH   PRLCERRA                                                         
         CLC   NDLCSYNM,SPACES     SYSTEM NAME REQUIRED                         
         BNH   PRLCERRB                                                         
*                                                                               
         MVI   BYTE,C'S'                                                        
         MVC   DUB(8),DUB                                                       
         MVC   DUB(6),NDLCSYCD                                                  
         BRAS  RE,VALMCS           VALIDATE MARKET/CABLE/SYSTEM                 
*        BNE   PRLCERRA            INVALID TALENT MARKET                        
*                                                                               
         MVC   SRTCLCD,NDLCCLCD    SET CLIENT CODE                              
         MVC   SRTPRCD,NDLCPRCD    SET PRODUCT CODE                             
         MVI   SRTMDCD,C'T'        MEDIA IS TELEVISION                          
*                                                                               
         MVC   SRTRECID,NDLCID     SET RECORD ID                                
*                                                                               
         MVC   SRTFLNM,WHDFLNM     SET UNIQUE FILE NAME                         
*                                                                               
         GOTOR DTEVAL,PARMS,NDLCASTR,SRTDATE  SET DATE                          
         BNZ   PRLCERR1               DATE IN ERROR                             
*                                                                               
         BRAS  RE,PUTSORT          ADD TO SORT                                  
*                                                                               
         GOTOR DTEVAL,PARMS,NDLCAEND,SRTDATE  SET END DATE                      
         BNZ   PRLCERR2               DATE IN ERROR                             
*                                                                               
         BRAS  RE,PUTSORT          ADD TO SORT                                  
*                                                                               
PRLCOKAY DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     PRLCX                                                            
*                                                                               
PRLCAGYI DS    0H                  INVALID ID CODE                              
         LAY   R2,ERRINVID                                                      
         B     PRLCERR                                                          
*                                                                               
PRLCAGYE DS    0H                  INVALID AGENCY CODE                          
         LAY   R2,ERRAGY                                                        
         B     PRLCERR                                                          
*                                                                               
PRLCERR1 DS    0H                  INVALID START DATE                           
         LAY   R2,ERRSTRDT                                                      
         B     PRLCERR                                                          
*                                                                               
PRLCERR2 DS    0H                  INVALID END DATE                             
         LAY   R2,ERRENDDT                                                      
         B     PRLCERR                                                          
*                                                                               
PRLCERR3 DS    0H                  INVALID COMMERCIAL                           
         LAY   R2,ERRCOM                                                        
         B     PRLCERR                                                          
*                                                                               
PRLCERR4 DS    0H                  INVALID COMMERCIAL LENGTH                    
         LAY   R2,ERRCOMLN                                                      
         B     PRLCERR                                                          
*                                                                               
PRLCERR5 DS    0H                  INVALID COMMERCIAL TITLE                     
         LAY   R2,ERRCOMTI                                                      
         B     PRLCERR                                                          
*                                                                               
PRLCERR6 DS    0H                  INVALID CLIENT CODE                          
         LAY   R2,ERRCLI                                                        
         B     PRLCERR                                                          
*                                                                               
PRLCERR7 DS    0H                  INVALID CLIENT NAME                          
         LAY   R2,ERRCLINM                                                      
         B     PRLCERR                                                          
*                                                                               
PRLCERR8 DS    0H                  INVALID PRODUCT CODE                         
         LAY   R2,ERRPRD                                                        
         B     PRLCERR                                                          
*                                                                               
PRLCERR9 DS    0H                  INVALID PRODUCT NAME                         
         LAY   R2,ERRPRDNM                                                      
         B     PRLCERR                                                          
*                                                                               
PRLCERRA DS    0H                  INVALID CABLE SYSTEM                         
         LAY   R2,ERRCSYS                                                       
         B     PRLCERR                                                          
*                                                                               
PRLCERRB DS    0H                  INVALID CABLE SYSTEM NAME                    
         LAY   R2,ERRCSYSN                                                      
         B     PRLCERR                                                          
*                                                                               
PRLCERR  DS    0H                  ERROR SKIP RECORD                            
*                                                                               
         GOTOR SMTPERR,PARMS,(0,0(R2)),(0,0(R3))  E-MAIL ERROR                  
*                                                                               
         LTR   RB,RB               SET NEQ CC                                   
*                                                                               
PRLCX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - SRLC'                                 
***********************************************************************         
*                                                                     *         
*        LOCAL CABLE                                                  *         
*                                                                     *         
*NTRY    R3==> UPLOAD RECORD                                          *         
*                                                                     *         
***********************************************************************         
                                                                                
SRLC     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,SRTDATA          ESTABLISH TV WILD SPOT DATA                  
         USING NDLCD,R3            ESTABLISH UPLOAD RECORD                      
*                                                                               
*        ADD TRANSFER ELEMENT TO RECORD                                         
*                                                                               
SRLCNX   DS    0H                                                               
*                                                                               
         BRAS  RE,INITWRK          INIT WORKER FILE AREA                        
         LA    R6,MYWORK           SET UP AS TRANSFER ELEMENT                   
*                                                                               
         USING TANXD,R6            ESTABLISH TRANSFER ELEMENT                   
*                                                                               
         XC    TANXEL(TANXLN2Q+1),TANXEL   INIT ELEMENT                         
*                                                                               
         MVI   TANXEL,TANXELQ      SET ELEMENT ID                               
         MVI   TANXLEN,TANXLNQ     SET ELEMENT LENGTH                           
         MVC   TANXAGY,SRTAGY      SET AGENCY CODE                              
         MVC   TANXUID,WHDMDCO     SET USER ID                                  
*                                                                               
         MVC   TANXNCID,NDLCCMID   SET COMMERCIAL ID                            
*                                                                               
         CLC   NDLCCMID+8(4),SPACES SKIP IF 8 CH CID                            
         BNH   SRLC10                                                           
*                                                                               
         GOTO1 ADTRPACK,DMCB,(C'P',NDLCCMID),TANXNCID PACK CID                  
         OI    TANXSTAT,TANXPACK   PACKED CID                                   
         MVI   TANXLEN,TANXLN2Q                                                 
         MVC   TANXADID,NDLCCMID                                                
*                                                                               
SRLC10   DS    0H                                                               
*                                                                               
         MVC   TANXSEC,SRTCMLN     SET COMMERCIAL LENGTH                        
         MVC   TANXUDTE,SRTDATE    SET DATE USED                                
         MVC   TANXADTE,WTODAY     SET DATE ADDED                               
         OI    TANXSTAT,TANXNDDS   NON-DDS CLARUS                               
         MVC   TANXMED,SRTMDCD     SET MEDIA                                    
*                                                                               
         DROP  R6                                                               
*                                                                               
SRLCNXX  DS    0H                                                               
*                                                                               
*        ADD   CLIENT CODE ELEMENT                                              
*                                                                               
SRLCCT   DS    0H                                                               
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TACTD,R6            ESTABLISH CLIENT CODE ELEMENT                
*                                                                               
         MVI   TACTEL,TACTELQ      CLIENT ELEMENT                               
         MVI   TACTLEN,TACTLNQ     ELEMENT LENGTH                               
         MVC   TACTCLI,NDLCCLCD    SET CLIENT CODE                              
         OC    TACTCLI,SPACES      FORCE UPPERCASE                              
*                                                                               
         DROP  R6                                                               
*                                                                               
SRLCCTX  DS    0H                                                               
*                                                                               
*        ADD   PRODUCT CODE ELEMENT                                             
*                                                                               
SRLCPR   DS    0H                                                               
*                                                                               
         CLC   NDLCPRCD,SPACES     IF UNALLOCATED                               
         BE    SRLCPRX             SKIP PROD CODE ELEMENT                       
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAPRD,R6            ESTABLISH AS PRODUCT CODE ELEMENT            
*                                                                               
         MVI   TAPREL,TAPRELQ      PRODUCT CODE ELEMENT                         
         MVI   TAPRLEN,TAPRLNQ     ELEMENT LENGTH                               
         MVC   TAPRPRD,NDLCPRCD    SET PROD CODE                                
         OC    TAPRPRD,SPACES      FORCE UPPERCASE                              
*                                                                               
         DROP  R6                                                               
*                                                                               
SRLCPRX  DS    0H                                                               
*                                                                               
*        ADD CLIENT NAME ELEMENT                                                
*                                                                               
SRLCCFN  DS    0H                                                               
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAFND,R6            ESTABLISH AS CLIENT NAME ELEMENT             
*                                                                               
         MVI   TAFNEL,TAFNELQ      SET ELEMENT ID                               
         MVI   TAFNTYPE,TAFNTCLI   SET AS CLIENT NAME ELM                       
         GOTO1 SQUASHER,DMCB,NDLCCLNM,L'NDLCCLNM                                
         L     RF,DMCB+4           GET LENGTH OF TITLE                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),NDLCCLNM SET CLIENT NAME                             
         AHI   RF,4                ADD FOR CORRECT ELEMENT LENGTH               
         STC   RF,TAFNLEN                                                       
*                                                                               
*        ADD PRODUCT NAME ELEMENT                                               
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAFND,R6            ESTABLISH AS PRODUCT NAME ELEMENT            
*                                                                               
         MVI   TAFNEL,TAFNELQ      SET ELEMENT ID                               
         MVI   TAFNTYPE,TAFNTPRD   SET AS PRODUCT NAME ELM                      
         GOTO1 SQUASHER,DMCB,NDLCPRNM,L'NDLCPRNM                                
         L     RF,DMCB+4           GET LENGTH OF TITLE                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),NDLCPRNM SET PRODUCT NAME                            
         AHI   RF,4                ADD FOR CORRECT ELEMENT LENGTH               
         STC   RF,TAFNLEN                                                       
         DROP  R6                                                               
*                                                                               
*        ADD COMMERCIAL TITLE ELEMENT                                           
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAFND,R6            ESTABLISH AS NAME ELEMENT                    
*                                                                               
         MVI   TAFNEL,TAFNELQ      SET ELEMENT ID                               
         MVI   TAFNTYPE,TAFNTTTL   SET AS COMMERCIAL TITLE ELM                  
         GOTO1 SQUASHER,DMCB,NDLCCMTL,L'NDLCCMTL                                
         L     RF,DMCB+4           GET LENGTH OF TITLE                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),NDLCCMTL SET COMMERCIAL TITLE                        
         AHI   RF,4                ADD FOR CORRECT ELEMENT LENGTH               
         STC   RF,TAFNLEN                                                       
         DROP  R6                                                               
*                                                                               
*        ADD PROGRAM DETAILS/MARKET ELEMENT TO RECORD                           
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TANPD,R6            ESTABLISH AS PROGRAM ELEMENT                 
*                                                                               
         MVI   TANPD,TANPELQ       SET ELEMENT TYPE                             
         MVI   TANPLEN,TANPLNQ4    SET ELEMENT LENGTH                           
         MVI   TANPSEQ,0           SET SEQUENCE NUMBER                          
         MVC   TANPDATE,SRTDATE    SET USE DATE                                 
         MVI   TANPTYP,TANPCSYS    ELEMENT TYPE - LOCAL CABLE                   
         MVC   TANPSYS,NDLCSYCD    SET CABLE SYSTEM CODE                        
         MVC   TANPMKTN,NDLCSYNM   SET CABLE SYSTEM NAME                        
*                                                                               
*        ADD MEDIA COMPANY NAME                                                 
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         GOTOR MEDCPYNM                                                         
*                                                                               
*        ADD UNIQUE FILE NAME ELEMENT                                           
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*&&DO                                                                           
         USING TACMD,R6            ESTABLISH AS COMMENT ELEMENT                 
*                                                                               
         MVI   TACMEL,TACMELQ      SET ELEMENT ID                               
         MVI   TACMTYPE,TACMTYPF   SET AS FILENAME COMMENT                      
         MVC   TACMCOMM(L'WHDFLNM),WHDFLNM SET FILW NAME                        
*                                                                               
         LA    RF,TACMCOMM+L'WHDFLNM-1 END OF FILE NAME                         
*                                                                               
         CLI   0(RF),C' '          FIND LAST CHARACTER OF NAME                  
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         LA    RF,1(RF)            POINT TO NEXT POSITION                       
         SR    RF,R6               LENGTH OF ELEMENT                            
         STC   RF,TACMLEN          SET ELEMENT LENGTH                           
*&&                                                                             
         GOTOR MOVEWRKR            MOVE ELMS TO WORKER IO AREA                  
*                                                                               
         GOTOR ADDWRKR             ADD DATA TO WORKER FILE                      
*                                                                               
SRLCUPDX DS    0H                                                               
*                                                                               
*        PRINT DETAIL LINE                                                      
*                                                                               
         CLI   TRACOPT,C'Y'        SKIP IF TRACING                              
         BE    SRLCOK                                                           
*                                                                               
         CLI   REPTOPT,C'Y'        SKIP IF NOT PRINTING REPORT                  
         BNE   SRLCOK                                                           
*                                                                               
         MVI   RCSUBPRG,28         DEFAULT PAGE TYPE                            
*                                                                               
         MVC   P,SPACES                                                         
         LA    R2,P                ESTABLISH PRINT LINE                         
         BRAS  RE,CNTR             CENTER REPORT                                
         USING PRWD,R2                                                          
*                                                                               
         MVC   PRWNID,SRTCMID      COMMERCIAL ID                                
         MVC   PRWTTL,NDLCCMTL     COMMERCIAL TITLE                             
         MVC   PRWNLEN,NDLCCMLN    SPOT LENGTH                                  
         MVC   PRWUSED,NDLCASTR    ACTIVE START DATE                            
         MVC   PRWMKTCD,NDLCSYCD   CABLE SYSTEM CODE                            
         MVC   PRWMKTNM,NDLCSYNM   CABLE SYSTEM NAME                            
*                                                                               
         BRAS  RE,SPLAT                                                         
*                                                                               
SRLCOK   DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     SRLCX                                                            
*                                                                               
SRLCERR  DS    0H                  ERROR SKIP RECORD                            
*                                                                               
SRLCERR1 DS    0H                  ERROR SKIP RECORD                            
*                                                                               
         CR    RB,RB                                                            
*                                                                               
SRLCX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - PRCB'                                 
***********************************************************************         
*                                                                     *         
*        NATWORK CABLE                                                *         
*                                                                     *         
*NTRY    R3==> UPLOAD RECORD                                          *         
*                                                                     *         
***********************************************************************         
                                                                                
PRCB     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   WBYPASS,C'Y'        SKIP IF BYPASSING BATCH                      
         BE    PRCBOKAY                                                         
*                                                                               
         USING NDCBD,R3            ESTABLISH UPLOAD RECORD                      
*                                                                               
         XC    SRTKEY(SRTHDRLN),SRTKEY  INIT SORT RECORD                        
         XC    SRTDATA,SRTDATA     INIT SORT RECORD                             
*                                                                               
         MVC   SRTDATA,0(R3)       COPY RECORD TO SORT RECORD                   
*                                                                               
         MVI   SRTTYPE,C'N'        NET RECORD TYPE                              
*                                                                               
         CLC   NDCBUPID,SPACES     UNIQUE ID REQUIRED                           
         BNH   PRCBAGYI                                                         
*                                                                               
*                                                                               
*        VERIFY AGENCY CODE IS ON FILE                                          
*                                                                               
         GOTOR AGYVAL,DMCB,NDCBAGY,WAGY  VALIDATE AGENCY                        
         BNE   PRCBAGYE               DROP IF AGENCY NOT VALID                  
*                                                                               
         MVC   SRTAGY,WAGY         SET AGENCY IN SORT KEY                       
*                                                                               
         CLC   NDCBCMID,SPACES     COMMERICAL ID REQUIRED                       
         BNH   PRCBERR3                                                         
*                                                                               
         MVC   SRTCMID,NDCBCMID    SET COMMERCIAL ID                            
*                                                                               
         MVC   DUB(3),NDCBCMLN     VALIDATE COMM LENGTH AS NUMERIC              
         NC    DUB(3),=8C'0'                                                    
         CLC   DUB(3),=8C'0'                                                    
         BNE   PRCBERR4               NOT NUMERIC                               
*                                                                               
         PACK  DUB,NDCBCMLN        CVD                                          
         CVB   RF,DUB              CVB                                          
         STC   RF,SRTCMLN          PASS LENGTH IN SORT RECORD                   
*                                                                               
         CLC   NDCBCMTL,SPACES     COMML TITLE REQUIRED                         
         BNH   PRCBERR5                                                         
         CLC   NDCBCLCD,SPACES     CLIENT CODE REQUIRED                         
         BNH   PRCBERR6                                                         
         CLC   NDCBCLNM,SPACES     CLIENT NAME REQUIRED                         
         BNH   PRCBERR7                                                         
         CLC   NDCBPRCD,SPACES     PRODUCT CODE REQUIRED                        
         BNH   PRCBERR8                                                         
         CLC   NDCBPRNM,SPACES     PRODUCT NAME REQUIRED                        
         BNH   PRCBERR9                                                         
         CLC   NDCBSTCD,SPACES     CABLE STATION REQUIRED                       
         BNH   PRCBERRA                                                         
         CLC   NDCBSTNM,SPACES     CABLE STATION NAME REQUIRED                  
         BNH   PRCBERRB                                                         
*                                                                               
         MVI   BYTE,C'N'                                                        
         MVC   DUB(8),DUB                                                       
         MVC   DUB(4),NDCBSTCD                                                  
         BRAS  RE,VALMCS           VALIDATE MARKET/CABLE/SYSTEM                 
*        BNE   PRCBERRA            INVALID STATION                              
*                                                                               
         MVC   SRTCLCD,NDCBCLCD    SET CLIENT CODE                              
         MVC   SRTPRCD,NDCBPRCD    SET PRODUCT CODE                             
         MVI   SRTMDCD,C'T'        MEDIA IS TELEVISION                          
*                                                                               
         MVC   SRTRECID,NDCBID     SET RECORD ID                                
*                                                                               
         GOTOR DTEVAL,PARMS,NDCBUDTE,SRTDATE  SET DATE                          
         BNZ   PRCBERR1               DATE IN ERROR                             
*                                                                               
         MVC   SRTFLNM,WHDFLNM     SET UNIQUE FILE NAME                         
*                                                                               
         BRAS  RE,PUTSORT          ADD TO SORT                                  
*                                                                               
PRCBOKAY DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     PRCBX                                                            
*                                                                               
PRCBAGYI DS    0H                  INVALID ID CODE                              
         LAY   R2,ERRINVID                                                      
         B     PRCBERR                                                          
*                                                                               
PRCBAGYE DS    0H                  INVALID AGENCY CODE                          
         LAY   R2,ERRAGY                                                        
         B     PRCBERR                                                          
*                                                                               
PRCBERR1 DS    0H                  INVALID USE DATE                             
         LAY   R2,ERRUSEDT                                                      
         B     PRCBERR                                                          
*                                                                               
PRCBERR3 DS    0H                  INVALID COMMERCIAL                           
         LAY   R2,ERRCOM                                                        
         B     PRCBERR                                                          
*                                                                               
PRCBERR4 DS    0H                  INVALID COMMERCIAL LENGTH                    
         LAY   R2,ERRCOMLN                                                      
         B     PRCBERR                                                          
*                                                                               
PRCBERR5 DS    0H                  INVALID COMMERCIAL TITLE                     
         LAY   R2,ERRCOMTI                                                      
         B     PRCBERR                                                          
*                                                                               
PRCBERR6 DS    0H                  INVALID CLIENT CODE                          
         LAY   R2,ERRCLI                                                        
         B     PRCBERR                                                          
*                                                                               
PRCBERR7 DS    0H                  INVALID CLIENT NAME                          
         LAY   R2,ERRCLINM                                                      
         B     PRCBERR                                                          
*                                                                               
PRCBERR8 DS    0H                  INVALID PRODUCT CODE                         
         LAY   R2,ERRPRD                                                        
         B     PRCBERR                                                          
*                                                                               
PRCBERR9 DS    0H                  INVALID PRODUCT NAME                         
         LAY   R2,ERRPRDNM                                                      
         B     PRCBERR                                                          
*                                                                               
PRCBERRA DS    0H                  INVALID CABLE STATION                        
         LAY   R2,ERRCSTA                                                       
         B     PRCBERR                                                          
*                                                                               
PRCBERRB DS    0H                  INVALID CABLE STATION NAME                   
         LAY   R2,ERRCSTAN                                                      
         B     PRCBERR                                                          
*                                                                               
PRCBERR  DS    0H                  ERROR SKIP RECORD                            
*                                                                               
         GOTOR SMTPERR,PARMS,(0,0(R2)),(0,0(R3))  E-MAIL ERROR                  
*                                                                               
         LTR   RB,RB                                                            
*                                                                               
PRCBX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - SRCB'                                 
***********************************************************************         
*                                                                     *         
*        LOCAL CABLE                                                  *         
*                                                                     *         
*NTRY    R3==> UPLOAD RECORD                                          *         
*                                                                     *         
***********************************************************************         
                                                                                
SRCB     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,SRTDATA          ESTABLISH TV WILD SPOT DATA                  
         USING NDCBD,R3            ESTABLISH UPLOAD RECORD                      
*                                                                               
*        ADD TRANSFER ELEMENT TO RECORD                                         
*                                                                               
SRCBNX   DS    0H                                                               
*                                                                               
         BRAS  RE,INITWRK          INIT WORKER FILE AREA                        
         LA    R6,MYWORK           SET UP AS TRANSFER ELEMENT                   
*                                                                               
         USING TANXD,R6            ESTABLISH TRANSFER ELEMENT                   
*                                                                               
         XC    TANXEL(TANXLN2Q+1),TANXEL   INIT ELEMENT                         
*                                                                               
         MVI   TANXEL,TANXELQ      SET ELEMENT ID                               
         MVI   TANXLEN,TANXLNQ     SET ELEMENT LENGTH                           
         MVC   TANXAGY,SRTAGY      SET AGENCY CODE                              
         MVC   TANXUID,WHDMDCO     SET USER ID                                  
*                                                                               
         MVC   TANXNCID,NDCBCMID   SET COMMERCIAL ID                            
*                                                                               
         CLC   NDCBCMID+8(4),SPACES SKIP IF 8 CH CID                            
         BNH   SRCB10                                                           
*                                                                               
         GOTO1 ADTRPACK,DMCB,(C'P',NDCBCMID),TANXNCID PACK CID                  
         OI    TANXSTAT,TANXPACK   PACKED CID                                   
         MVI   TANXLEN,TANXLN2Q                                                 
         MVC   TANXADID,NDCBCMID                                                
*                                                                               
SRCB10   DS    0H                                                               
*                                                                               
         MVC   TANXSEC,SRTCMLN     SET COMMERCIAL LENGTH                        
         MVC   TANXUDTE,SRTDATE    SET DATE USED                                
         MVC   TANXADTE,WTODAY     SET DATE ADDED                               
         OI    TANXSTAT,TANXNDDS   NON-DDS CLARUS                               
         MVC   TANXMED,SRTMDCD     SET MEDIA                                    
*                                                                               
         DROP  R6                                                               
*                                                                               
SRCBNXX  DS    0H                                                               
*                                                                               
*        ADD   CLIENT CODE ELEMENT                                              
*                                                                               
SRCBCT   DS    0H                                                               
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TACTD,R6            ESTABLISH CLIENT CODE ELEMENT                
*                                                                               
         MVI   TACTEL,TACTELQ      CLIENT ELEMENT                               
         MVI   TACTLEN,TACTLNQ     ELEMENT LENGTH                               
         MVC   TACTCLI,NDCBCLCD    SET CLIENT CODE                              
         OC    TACTCLI,SPACES      FORCE UPPERCASE                              
*                                                                               
         DROP  R6                                                               
*                                                                               
SRCBCTX  DS    0H                                                               
*                                                                               
*        ADD   PRODUCT CODE ELEMENT                                             
*                                                                               
SRCBPR   DS    0H                                                               
*                                                                               
         CLC   NDCBPRCD,SPACES     IF UNALLOCATED                               
         BE    SRCBPRX             SKIP PROD CODE ELEMENT                       
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAPRD,R6            ESTABLISH AS PRODUCT CODE ELEMENT            
*                                                                               
         MVI   TAPREL,TAPRELQ      PRODUCT CODE ELEMENT                         
         MVI   TAPRLEN,TAPRLNQ     ELEMENT LENGTH                               
         MVC   TAPRPRD,NDCBPRCD    SET PROD CODE                                
         OC    TAPRPRD,SPACES      FORCE UPPERCASE                              
*                                                                               
         DROP  R6                                                               
*                                                                               
SRCBPRX  DS    0H                                                               
*                                                                               
*        ADD CLIENT NAME ELEMENT                                                
*                                                                               
SRCBCFN  DS    0H                                                               
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAFND,R6            ESTABLISH AS CLIENT NAME ELEMENT             
*                                                                               
         MVI   TAFNEL,TAFNELQ      SET ELEMENT ID                               
         MVI   TAFNTYPE,TAFNTCLI   SET AS CLIENT NAME ELM                       
         GOTO1 SQUASHER,DMCB,NDCBCLNM,L'NDCBCLNM                                
         L     RF,DMCB+4           GET LENGTH OF TITLE                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),NDCBCLNM SET CLIENT NAME                             
         AHI   RF,4                ADD FOR CORRECT ELEMENT LENGTH               
         STC   RF,TAFNLEN                                                       
*                                                                               
*        ADD PRODUCT NAME ELEMENT                                               
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAFND,R6            ESTABLISH AS PRODUCT NAME ELEMENT            
*                                                                               
         MVI   TAFNEL,TAFNELQ      SET ELEMENT ID                               
         MVI   TAFNTYPE,TAFNTPRD   SET AS PRODUCT NAME ELM                      
         GOTO1 SQUASHER,DMCB,NDCBPRNM,L'NDCBPRNM                                
         L     RF,DMCB+4           GET LENGTH OF TITLE                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),NDCBPRNM SET PRODUCT NAME                            
         AHI   RF,4                ADD FOR CORRECT ELEMENT LENGTH               
         STC   RF,TAFNLEN                                                       
         DROP  R6                                                               
*                                                                               
*        ADD COMMERCIAL TITLE ELEMENT                                           
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAFND,R6            ESTABLISH AS NAME ELEMENT                    
*                                                                               
         MVI   TAFNEL,TAFNELQ      SET ELEMENT ID                               
         MVI   TAFNTYPE,TAFNTTTL   SET AS COMMERCIAL TITLE ELM                  
         GOTO1 SQUASHER,DMCB,NDCBCMTL,L'NDCBCMTL                                
         L     RF,DMCB+4           GET LENGTH OF TITLE                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),NDCBCMTL SET COMMERCIAL TITLE                        
         AHI   RF,4                ADD FOR CORRECT ELEMENT LENGTH               
         STC   RF,TAFNLEN                                                       
         DROP  R6                                                               
*                                                                               
*        ADD PROGRAM DETAILS/MARKET ELEMENT TO RECORD                           
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TANPD,R6            ESTABLISH AS PROGRAM ELEMENT                 
*                                                                               
         MVI   TANPD,TANPELQ       SET ELEMENT TYPE                             
         MVI   TANPLEN,TANPLNQ4    SET ELEMENT LENGTH                           
         MVI   TANPSEQ,0           SET SEQUENCE NUMBER                          
         MVC   TANPDATE,SRTDATE    SET USE DATE                                 
         MVI   TANPTYP,TANPNET     NETWORK CABLE                                
         MVC   TANPNTI,NDCBSTCD    SET CABLE STATION CODE                       
         MVC   TANPMKTN,NDCBSTNM   SET CABLE STATION NAME                       
*                                                                               
*        ADD MEDIA COMPANY NAME                                                 
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         GOTOR MEDCPYNM                                                         
*                                                                               
*        ADD UNIQUE FILE NAME ELEMENT                                           
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*&&DO                                                                           
         USING TACMD,R6            ESTABLISH AS COMMENT ELEMENT                 
*                                                                               
         MVI   TACMEL,TACMELQ      SET ELEMENT ID                               
         MVI   TACMTYPE,TACMTYPF   SET AS FILENAME COMMENT                      
         MVC   TACMCOMM(L'WHDFLNM),WHDFLNM SET FILW NAME                        
*                                                                               
         LA    RF,TACMCOMM+L'WHDFLNM-1 END OF FILE NAME                         
*                                                                               
         CLI   0(RF),C' '          FIND LAST CHARACTER OF NAME                  
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         LA    RF,1(RF)            POINT TO NEXT POSITION                       
         SR    RF,R6               LENGTH OF ELEMENT                            
         STC   RF,TACMLEN          SET ELEMENT LENGTH                           
*&&                                                                             
         GOTOR MOVEWRKR            MOVE ELMS TO WORKER IO AREA                  
*                                                                               
         GOTOR ADDWRKR             ADD DATA TO WORKER FILE                      
*                                                                               
SRCBUPDX DS    0H                                                               
*                                                                               
*        PRINT DETAIL LINE                                                      
*                                                                               
         CLI   TRACOPT,C'Y'        SKIP IF TRACING                              
         BE    SRCBOK                                                           
*                                                                               
         CLI   REPTOPT,C'Y'        SKIP IF NOT PRINTING REPORT                  
         BNE   SRCBOK                                                           
*                                                                               
         MVI   RCSUBPRG,28         DEFAULT PAGE TYPE                            
*                                                                               
         MVC   P,SPACES                                                         
         LA    R2,P                ESTABLISH PRINT LINE                         
         BRAS  RE,CNTR             CENTER REPORT                                
         USING PRWD,R2                                                          
*                                                                               
         MVC   PRWNID,SRTCMID      COMMERCIAL ID                                
         MVC   PRWTTL,NDCBCMTL     COMMERCIAL TITLE                             
         MVC   PRWNLEN,NDCBCMLN    SPOT LENGTH                                  
         MVC   PRWUSED,NDCBUDTE    ACTIVE START DATE                            
         MVC   PRWMKTCD,NDCBSTCD   CABLE SYSTEM CODE                            
         MVC   PRWMKTNM,NDCBSTNM   CABLE SYSTEM NAME                            
*                                                                               
         BRAS  RE,SPLAT                                                         
*                                                                               
SRCBOK   DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     SRCBX                                                            
*                                                                               
SRCBERR  DS    0H                  ERROR SKIP RECORD                            
*                                                                               
SRCBERR1 DS    0H                  ERROR SKIP RECORD                            
*                                                                               
         CR    RB,RB                                                            
*                                                                               
SRCBX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - PRNT'                                 
***********************************************************************         
*                                                                     *         
*        NETWORK                                                      *         
*                                                                     *         
*NTRY    R3==> UPLOAD RECORD                                          *         
*                                                                     *         
***********************************************************************         
                                                                                
PRNT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   WBYPASS,C'Y'        SKIP IF BYPASSING BATCH                      
         BE    PRNTOKAY                                                         
*                                                                               
         USING NDNTD,R3            ESTABLISH UPLOAD RECORD                      
*                                                                               
         XC    SRTKEY(SRTHDRLN),SRTKEY  INIT SORT RECORD                        
         XC    SRTDATA,SRTDATA     INIT SORT RECORD                             
*                                                                               
         MVC   SRTDATA,0(R3)       COPY RECORD TO SORT RECORD                   
*                                                                               
         MVI   SRTTYPE,C'N'        NET RECORD TYPE                              
*                                                                               
         CLC   NDNTUPID,SPACES     UNIQUE ID REQUIRED                           
         BNH   PRNTAGYI                                                         
*                                                                               
*        VERIFY AGENCY CODE IS ON FILE                                          
*                                                                               
         GOTOR AGYVAL,DMCB,NDNTAGY,WAGY  VALIDATE AGENCY                        
         BNE   PRNTAGYE               DROP IF AGENCY NOT VALID                  
*                                                                               
         MVC   SRTAGY,WAGY         SET AGENCY IN SORT KEY                       
*                                                                               
         CLC   NDNTCMID,SPACES     COMMERICAL ID REQUIRED                       
         BNH   PRNTERR3                                                         
*                                                                               
         MVC   SRTCMID,NDNTCMID    SET COMMERCIAL ID                            
*                                                                               
         MVC   DUB(3),NDNTCMLN     VALIDATE COMM LENGTH AS NUMERIC              
         NC    DUB(3),=8C'0'                                                    
         CLC   DUB(3),=8C'0'                                                    
         BNE   PRNTERR4               NOT NUMERIC                               
*                                                                               
         PACK  DUB,NDNTCMLN        CVD                                          
         CVB   RF,DUB              CVB                                          
         STC   RF,SRTCMLN          PASS LENGTH IN SORT RECORD                   
*                                                                               
         CLC   NDNTCMTL,SPACES     COMML TITLE REQUIRED                         
         BNH   PRNTERR5                                                         
         CLC   NDNTCLCD,SPACES     CLIENT CODE REQUIRED                         
         BNH   PRNTERR6                                                         
         CLC   NDNTCLNM,SPACES     CLIENT NAME REQUIRED                         
         BNH   PRNTERR7                                                         
         CLC   NDNTPRCD,SPACES     PRODUCT CODE REQUIRED                        
         BNH   PRNTERR8                                                         
         CLC   NDNTPRNM,SPACES     PRODUCT NAME REQUIRED                        
         BNH   PRNTERR9                                                         
         CLC   NDNTPGNM,SPACES     PROGRAM NAME REQUIRED                        
         BNH   PRNTERR2                                                         
*                                                                               
         LA    RF,NTCDTAB          POINT TO NETWORK CODE TABLE                  
PRNT3    CLI   0(RF),X'FF'         EOT?                                         
         BE    PRNTERRA            YES, ERROR                                   
         CLC   NDNTNTCD,0(RF)      MATCH ON NETWORK CODE                        
         BE    PRNT5               YES, CONTINUE                                
         AHI   RF,1                                                             
         B     PRNT3                                                            
*                                                                               
PRNT5    LA    RF,NDNTUNMK         UNMARKED                                     
         BAS   RE,VALTRFLS                                                      
         BNE   PRNTER11                                                         
         LA    RF,NDNTNAIR         UNAIRED                                      
         BAS   RE,VALTRFLS                                                      
         BNE   PRNTER12                                                         
         LA    RF,NDNTLCHG         LENGTH CHANGED                               
         BAS   RE,VALTRFLS                                                      
         BNE   PRNTER13                                                         
         LA    RF,NDNTPCHG         PRODUCT CHANGED                              
         BAS   RE,VALTRFLS                                                      
         BNE   PRNTER14                                                         
         LA    RF,NDNTDCHG         DATE CHANGED                                 
         BAS   RE,VALTRFLS                                                      
         BNE   PRNTER15                                                         
         LA    RF,NDNTCCHG         COMMERCIAL CHANGED                           
         BAS   RE,VALTRFLS                                                      
         BNE   PRNTER16                                                         
         LA    RF,NDNTFLAT         FLAT RATE/LATE NIGHT                         
         BAS   RE,VALTRFLS                                                      
         BNE   PRNTER17                                                         
         CLI   NDNTNWHY,C' '       REASON NOT AIRED, BLANK OK                   
         BNE   PRNT6                                                            
         CLI   NDNTNAIR,C'T'       THEN NOT AIRED CAN'T BE TRUE                 
         BE    PRNTER18                                                         
         B     PRNT8                                                            
*                                                                               
PRNT6    CLI   NDNTNWHY,C'D'       REASON NOT AIRED, DELETED                    
         BE    PRNT7                                                            
         CLI   NDNTNWHY,C'P'         PRE-EMPTED                                 
         BE    PRNT7                                                            
         CLI   NDNTNWHY,C'M'         MISSED                                     
         BNE   PRNTER18                                                         
PRNT7    CLI   NDNTNAIR,C'T'       NOT AIRED MUST BE TRUE                       
         BNE   PRNTER18                                                         
*                                                                               
PRNT8    LA    RF,NDNTMRUN         MULITPLE RUN                                 
         BAS   RE,VALTRFLS                                                      
         BNE   PRNTER19                                                         
                                                                                
         MVC   SRTCLCD,NDNTCLCD    SET CLIENT CODE                              
         MVC   SRTPRCD,NDNTPRCD    SET PRODUCT CODE                             
         MVI   SRTMDCD,C'T'        MEDIA IS TELEVISION                          
*                                                                               
         MVC   SRTRECID,NDNTID     SET RECORD ID                                
*                                                                               
         GOTOR DTEVAL,PARMS,NDNTUDTE,SRTDATE  SET DATE                          
         BNZ   PRNTERR1               DATE IN ERROR                             
*                                                                               
         MVC   SRTFLNM,WHDFLNM     SET UNIQUE FILE NAME                         
*                                                                               
         BRAS  RE,PUTSORT          ADD TO SORT                                  
*                                                                               
PRNTOKAY DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     PRNTX                                                            
*                                                                               
PRNTAGYI DS    0H                  INVALID ID CODE                              
         LAY   R2,ERRINVID                                                      
         B     PRNTERR                                                          
*                                                                               
PRNTAGYE DS    0H                  INVALID AGENCY CODE                          
         LAY   R2,ERRAGY                                                        
         B     PRNTERR                                                          
*                                                                               
PRNTERR1 DS    0H                  INVALID USE DATE                             
         LAY   R2,ERRUSEDT                                                      
         B     PRNTERR                                                          
*                                                                               
PRNTERR2 DS    0H                  INVALID PROGRAM NAME                         
         LAY   R2,ERRPRGNM                                                      
         B     PRNTERR                                                          
*                                                                               
PRNTERR3 DS    0H                  INVALID COMMERCIAL                           
         LAY   R2,ERRCOM                                                        
         B     PRNTERR                                                          
*                                                                               
PRNTERR4 DS    0H                  INVALID COMMERCIAL LENGTH                    
         LAY   R2,ERRCOMLN                                                      
         B     PRNTERR                                                          
*                                                                               
PRNTERR5 DS    0H                  INVALID COMMERCIAL TITLE                     
         LAY   R2,ERRCOMTI                                                      
         B     PRNTERR                                                          
*                                                                               
PRNTERR6 DS    0H                  INVALID CLIENT CODE                          
         LAY   R2,ERRCLI                                                        
         B     PRNTERR                                                          
*                                                                               
PRNTERR7 DS    0H                  INVALID CLIENT NAME                          
         LAY   R2,ERRCLINM                                                      
         B     PRNTERR                                                          
*                                                                               
PRNTERR8 DS    0H                  INVALID PRODUCT CODE                         
         LAY   R2,ERRPRD                                                        
         B     PRNTERR                                                          
*                                                                               
PRNTERR9 DS    0H                  INVALID PRODUCT NAME                         
         LAY   R2,ERRPRDNM                                                      
         B     PRNTERR                                                          
*                                                                               
PRNTERRA DS    0H                  INVALID NETWORK CODE                         
         LAY   R2,ERRNETCD                                                      
         B     PRNTERR                                                          
*                                                                               
PRNTER11 DS    0H                  INVALID UNMARKED                             
         LAY   R2,ERRUNMRK                                                      
         B     PRNTERR                                                          
*                                                                               
PRNTER12 DS    0H                  INVALID NOT AIRED                            
         LAY   R2,ERRNTAIR                                                      
         B     PRNTERR                                                          
*                                                                               
PRNTER13 DS    0H                  INVALID LENGTH CHANGED                       
         LAY   R2,ERRLENCH                                                      
         B     PRNTERR                                                          
*                                                                               
PRNTER14 DS    0H                  INVALID PRODUCT CHANGED                      
         LAY   R2,ERRPRDCH                                                      
         B     PRNTERR                                                          
*                                                                               
PRNTER15 DS    0H                  INVALID DATE CHANGED                         
         LAY   R2,ERRDTECH                                                      
         B     PRNTERR                                                          
*                                                                               
PRNTER16 DS    0H                  INVALID COMMERCIAL CHANGED                   
         LAY   R2,ERRCOMCH                                                      
         B     PRNTERR                                                          
*                                                                               
PRNTER17 DS    0H                  INVALID FLAT RATE/LATE NIGHT                 
         LAY   R2,ERRFLLTR                                                      
         B     PRNTERR                                                          
*                                                                               
PRNTER18 DS    0H                  INVALID REASON NOT AIRED                     
         LAY   R2,ERRREASN                                                      
         B     PRNTERR                                                          
*                                                                               
PRNTER19 DS    0H                  INVALID MULTIPLE RUN                         
         LAY   R2,ERRMULTR                                                      
         B     PRNTERR                                                          
*                                                                               
PRNTERR  DS    0H                  ERROR SKIP RECORD                            
*                                                                               
         GOTOR SMTPERR,PARMS,(0,0(R2)),(0,0(R3))  E-MAIL ERROR                  
*                                                                               
         LTR   RB,RB                                                            
*                                                                               
PRNTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
NTCDTAB  DC    C'ACFIMNXUWCSBEYZPGRJLTDKQOH'                                    
         DC    X'FF'                                                            
*                                                                               
VALTRFLS CLI   0(RF),C'T'          VALIDATES T/F ENTRY                          
         BER   RE                                                               
         CLI   0(RF),C'F'                                                       
         BER   RE                                                               
         CLI   0(RF),C' '                                                       
         BER   RE                                                               
         BNER  RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - SRNT'                                 
***********************************************************************         
*                                                                     *         
*        NETWORK                                                      *         
*                                                                     *         
*NTRY    R3==> UPLOAD RECORD                                          *         
*                                                                     *         
***********************************************************************         
                                                                                
SRNT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,SRTDATA          ESTABLISH TV WILD SPOT DATA                  
         USING NDNTD,R3            ESTABLISH UPLOAD RECORD                      
*                                                                               
*        ADD TRANSFER ELEMENT TO RECORD                                         
*                                                                               
SRNTNX   DS    0H                                                               
*                                                                               
         BRAS  RE,INITWRK          INIT WORKER FILE AREA                        
         LA    R6,MYWORK           SET UP AS TRANSFER ELEMENT                   
*                                                                               
         USING TANXD,R6            ESTABLISH TRANSFER ELEMENT                   
*                                                                               
         XC    TANXEL(TANXLN2Q+1),TANXEL   INIT ELEMENT                         
*                                                                               
         MVI   TANXEL,TANXELQ      SET ELEMENT ID                               
         MVI   TANXLEN,TANXLNQ     SET ELEMENT LENGTH                           
         MVC   TANXAGY,SRTAGY      SET AGENCY CODE                              
         MVC   TANXUID,WHDMDCO     SET USER ID                                  
*                                                                               
         MVC   TANXNCID,NDNTCMID   SET COMMERCIAL ID                            
*                                                                               
         CLC   NDNTCMID+8(4),SPACES SKIP IF 8 CH CID                            
         BNH   SRNT10                                                           
*                                                                               
         GOTO1 ADTRPACK,DMCB,(C'P',NDNTCMID),TANXNCID PACK CID                  
         OI    TANXSTAT,TANXPACK   PACKED CID                                   
         MVI   TANXLEN,TANXLN2Q                                                 
         MVC   TANXADID,NDNTCMID                                                
*                                                                               
SRNT10   DS    0H                                                               
*                                                                               
         MVC   TANXSEC,SRTCMLN     SET COMMERCIAL LENGTH                        
         MVC   TANXUDTE,SRTDATE    SET DATE USED                                
         MVC   TANXADTE,WTODAY     SET DATE ADDED                               
*                                                                               
         CLI   NDNTUNMK,C'T'       CHECK FOR UNMARKED                           
         BNE   *+8                                                              
         OI    TANXCCDE,TANXCUNM      SET STATUS                                
*                                                                               
         CLI   NDNTNAIR,C'T'       CHECK FOR NOT AIRED                          
         BNE   *+8                                                              
         OI    TANXCCDE,TANXCAIR      SET STATUS                                
*                                                                               
         CLI   NDNTLCHG,C'T'       CHECK FOR LENGTH CHANGED                     
         BNE   *+8                                                              
         OI    TANXCCDE,TANXCLEN      SET STATUS                                
*                                                                               
         CLI   NDNTPCHG,C'T'       CHECK FOR PRODUCT CHANGED                    
         BNE   *+8                                                              
         OI    TANXCCDE,TANXCPRD      SET STATUS                                
*                                                                               
         CLI   NDNTDCHG,C'T'       CHECK FOR DATE CHANGED                       
         BNE   *+8                                                              
         OI    TANXCCDE,TANXCDTE      SET STATUS                                
*                                                                               
         CLI   NDNTCCHG,C'T'       CHECK FOR COMMERCIAL CHANGED                 
         BNE   *+8                                                              
         OI    TANXCCDE,TANXCCID      SET STATUS                                
*                                                                               
         OI    TANXSTAT,TANXNDDS   NON-DDS CLARUS                               
         MVC   TANXMED,SRTMDCD     SET MEDIA                                    
*                                                                               
         DROP  R6                                                               
*                                                                               
SRNTNXX  DS    0H                                                               
*                                                                               
*        ADD   CLIENT CODE ELEMENT                                              
*                                                                               
SRNTCT   DS    0H                                                               
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TACTD,R6            ESTABLISH CLIENT CODE ELEMENT                
*                                                                               
         MVI   TACTEL,TACTELQ      CLIENT ELEMENT                               
         MVI   TACTLEN,TACTLNQ     ELEMENT LENGTH                               
         MVC   TACTCLI,NDNTCLCD    SET CLIENT CODE                              
         OC    TACTCLI,SPACES      FORCE UPPERCASE                              
*                                                                               
         DROP  R6                                                               
*                                                                               
SRNTCTX  DS    0H                                                               
*                                                                               
*        ADD   PRODUCT CODE ELEMENT                                             
*                                                                               
SRNTPR   DS    0H                                                               
*                                                                               
         CLC   NDNTPRCD,SPACES     IF UNALLOCATED                               
         BE    SRNTPRX             SKIP PROD CODE ELEMENT                       
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAPRD,R6            ESTABLISH AS PRODUCT CODE ELEMENT            
*                                                                               
         MVI   TAPREL,TAPRELQ      PRODUCT CODE ELEMENT                         
         MVI   TAPRLEN,TAPRLNQ     ELEMENT LENGTH                               
         MVC   TAPRPRD,NDNTPRCD    SET PROD CODE                                
         OC    TAPRPRD,SPACES      FORCE UPPERCASE                              
*                                                                               
         DROP  R6                                                               
*                                                                               
SRNTPRX  DS    0H                                                               
*                                                                               
*        ADD CLIENT NAME ELEMENT                                                
*                                                                               
SRNTCFN  DS    0H                                                               
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAFND,R6            ESTABLISH AS CLIENT NAME ELEMENT             
*                                                                               
         MVI   TAFNEL,TAFNELQ      SET ELEMENT ID                               
         MVI   TAFNTYPE,TAFNTCLI   SET AS CLIENT NAME ELM                       
         GOTO1 SQUASHER,DMCB,NDNTCLNM,L'NDNTCLNM                                
         L     RF,DMCB+4           GET LENGTH OF TITLE                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),NDNTCLNM SET CLIENT NAME                             
         AHI   RF,4                ADD FOR CORRECT ELEMENT LENGTH               
         STC   RF,TAFNLEN                                                       
*                                                                               
*        ADD PRODUCT NAME ELEMENT                                               
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAFND,R6            ESTABLISH AS PRODUCT NAME ELEMENT            
*                                                                               
         MVI   TAFNEL,TAFNELQ      SET ELEMENT ID                               
         MVI   TAFNTYPE,TAFNTPRD   SET AS PRODUCT NAME ELM                      
         GOTO1 SQUASHER,DMCB,NDNTPRNM,L'NDNTPRNM                                
         L     RF,DMCB+4           GET LENGTH OF TITLE                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),NDNTPRNM SET PRODUCT NAME                            
         AHI   RF,4                ADD FOR CORRECT ELEMENT LENGTH               
         STC   RF,TAFNLEN                                                       
         DROP  R6                                                               
*                                                                               
*        ADD COMMERCIAL TITLE ELEMENT                                           
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TAFND,R6            ESTABLISH AS NAME ELEMENT                    
*                                                                               
         MVI   TAFNEL,TAFNELQ      SET ELEMENT ID                               
         MVI   TAFNTYPE,TAFNTTTL   SET AS COMMERCIAL TITLE ELM                  
         GOTO1 SQUASHER,DMCB,NDNTCMTL,L'NDNTCMTL                                
         L     RF,DMCB+4           GET LENGTH OF TITLE                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),NDNTCMTL SET COMMERCIAL TITLE                        
         AHI   RF,4                ADD FOR CORRECT ELEMENT LENGTH               
         STC   RF,TAFNLEN                                                       
*                                                                               
*&&DO                                                                           
         LA    RF,TAFNNAME+L'NDTWCMTL-1 END OF NAME                             
*                                                                               
         CLI   0(RF),C' '          FIND LAST CHARACTER OF NAME                  
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         LA    RF,1(RF)            POINT TO NEXT POSITION                       
         SR    RF,R6               LENGTH OF ELEMENT                            
         STC   RF,TAFNLEN          SET ELEMENT LENGTH                           
*&&                                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
*        ADD PROGRAM DETAILS/MARKET ELEMENT TO RECORD                           
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         USING TANPD,R6            ESTABLISH AS PROGRAM ELEMENT                 
*                                                                               
         MVI   TANPD,TANPELQ       SET ELEMENT TYPE                             
         MVI   TANPLEN,TANPLNQ3    SET ELEMENT LENGTH                           
         MVI   TANPSEQ,0           SET SEQUENCE NUMBER                          
         MVC   TANPDATE,SRTDATE    SET USE DATE                                 
         MVC   TANPPNME,NDNTPGNM   SET PROGRAM NAME                             
         MVC   TANPNWK,NDNTNTCD    SET NETWORK CODE                             
******   MVI   TANPTYP,TANPSPT     ELEMENT TYPE                                 
*                                                                               
*        SET STATUS BIT                                                         
*                                                                               
**NO-OP  CLI   NDNTNTCD,C'F'       IF FOX NETWORK, SKIP LATE NIGHT              
**11/16  BE    SKIPLATE            (TEMPORARY)                                  
*                                                                               
         CLI   NDNTNTCD,C'A'       IF NETWORK ABC,                              
         BE    CHKLATE                                                          
         CLI   NDNTNTCD,C'N'       NBC,                                         
         BE    CHKLATE                                                          
         CLI   NDNTNTCD,C'C'       CBS,                                         
         BE    CHKLATE                                                          
         CLI   NDNTNTCD,C'F'       OR FOX                                       
         BNE   SKIPLATE                                                         
CHKLATE  CLI   NDNTFLAT,C'T'       CHECK FOR FLAT RATE/LATE NIGHT               
         BNE   *+8                                                              
         OI    TANPSTAT,TANPFLAT      SET STATUS                                
*                                                                               
SKIPLATE DS    0H                                                               
         CLI   NDNTMRUN,C'T'       CHECK FOR MULTIPLE RUN                       
         BNE   *+8                                                              
         OI    TANPSTAT,TANPMR        SET STATUS                                
*                                                                               
         CLI   NDNTNTCD,C'X'       CHECK FOR PAX NETWORK                        
         BNE   *+8                                                              
         OI    TANPSTAT,TANPPAX       SET STATUS                                
*                                                                               
         CLI   NDNTNTCD,C'B'       BOUNCE WORKS LIKE PAX                        
         BNE   *+8                                                              
         OI    TANPSTAT,TANPPAX       SET STATUS                                
*                                                                               
         CLI   NDNTNTCD,C'E'       METV WORKS LIKE PAX                          
         BNE   *+8                                                              
         OI    TANPSTAT,TANPPAX       SET STATUS                                
*                                                                               
         CLI   NDNTNTCD,C'Y'       ANTENNA WORKS LIKE PAX                       
         BNE   *+8                                                              
         OI    TANPSTAT,TANPPAX       SET STATUS                                
*                                                                               
         CLI   NDNTNTCD,C'Z'       COZITV WORKS LIKE PAX                        
         BNE   *+8                                                              
         OI    TANPSTAT,TANPPAX       SET STATUS                                
*                                                                               
         CLI   NDNTNTCD,C'P'       ESCAPE WORKS LIKE PAX                        
         BNE   *+8                                                              
         OI    TANPSTAT,TANPPAX       SET STATUS                                
*                                                                               
         CLI   NDNTNTCD,C'G'       GETTV WORKS LIKE PAX                         
         BNE   *+8                                                              
         OI    TANPSTAT,TANPPAX       SET STATUS                                
*                                                                               
         CLI   NDNTNTCD,C'R'       GRIT WORKS LIKE PAX                          
         BNE   *+8                                                              
         OI    TANPSTAT,TANPPAX       SET STATUS                                
*                                                                               
         CLI   NDNTNTCD,C'J'       JUST WORKS LIKE PAX                          
         BNE   *+8                                                              
         OI    TANPSTAT,TANPPAX       SET STATUS                                
*                                                                               
         CLI   NDNTNTCD,C'L'       LAFF WORKS LIKE PAX                          
         BNE   *+8                                                              
         OI    TANPSTAT,TANPPAX       SET STATUS                                
*                                                                               
         CLI   NDNTNTCD,C'T'       THISTV WORKS LIKE PAX                        
         BNE   *+8                                                              
         OI    TANPSTAT,TANPPAX       SET STATUS                                
*                                                                               
         CLI   NDNTNTCD,C'I'       CHECK FOR ITN NETWORK                        
         BNE   *+8                                                              
         OI    TANPSTAT,TANPITN       SET STATUS                                
*                                                                               
         CLI   NDNTNTCD,C'D'       ACTIVE WORKS LIKE ITN                        
         BNE   *+8                                                              
         OI    TANPSTAT,TANPITN       SET STATUS                                
*                                                                               
         CLI   NDNTNTCD,C'K'       CONTINUUM WORKS LIKE ITN                     
         BNE   *+8                                                              
         OI    TANPSTAT,TANPITN       SET STATUS                                
*                                                                               
         CLI   NDNTNTCD,C'Q'       REVSHARE WORKS LIKE ITN                      
         BNE   *+8                                                              
         OI    TANPSTAT,TANPITN       SET STATUS                                
*                                                                               
         CLI   NDNTNTCD,C'O'       ICON WORKS LIKE ITN                          
         BNE   *+8                                                              
         OI    TANPSTAT,TANPITN       SET STATUS                                
*                                                                               
         CLI   NDNTNTCD,C'H'       CADENT WORKS LIKE ITN                        
         BNE   *+8                                                              
         OI    TANPSTAT,TANPITN       SET STATUS                                
*                                                                               
         CLI   NDNTNWHY,C' '                                                    
         BNH   *+10                                                             
         MVC   TANPAIR,NDNTNWHY    SET NOT AIRED REASON                         
*                                                                               
*        ADD MEDIA COMPANY NAME                                                 
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*                                                                               
         GOTOR MEDCPYNM                                                         
*                                                                               
*        ADD UNIQUE FILE NAME ELEMENT                                           
*                                                                               
         LLC   RF,1(R6)            BUMP TO NEXT ELEMENT AREA                    
         LA    R6,0(RF,R6)                                                      
*&&DO                                                                           
         USING TACMD,R6            ESTABLISH AS COMMENT ELEMENT                 
*                                                                               
         MVI   TACMEL,TACMELQ      SET ELEMENT ID                               
         MVI   TACMTYPE,TACMTYPF   SET AS FILENAME COMMENT                      
         MVC   TACMCOMM(L'WHDFLNM),WHDFLNM SET FILW NAME                        
*                                                                               
         LA    RF,TACMCOMM+L'WHDFLNM-1 END OF FILE NAME                         
*                                                                               
         CLI   0(RF),C' '          FIND LAST CHARACTER OF NAME                  
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         LA    RF,1(RF)            POINT TO NEXT POSITION                       
         SR    RF,R6               LENGTH OF ELEMENT                            
         STC   RF,TACMLEN          SET ELEMENT LENGTH                           
*&&                                                                             
         GOTOR MOVEWRKR            MOVE ELMS TO WORKER IO AREA                  
*                                                                               
         GOTOR ADDWRKR             ADD DATA TO WORKER FILE                      
*                                                                               
*        PRINT DETAIL LINE                                                      
*                                                                               
         CLI   TRACOPT,C'Y'        SKIP IF TRACING                              
         BE    SRNTOK                                                           
*                                                                               
         CLI   REPTOPT,C'Y'        SKIP IF NOT PRINTING REPORT                  
         BNE   SRNTOK                                                           
*                                                                               
         MVI   RCSUBPRG,28         DEFAULT PAGE TYPE                            
*                                                                               
         MVC   P,SPACES                                                         
         LA    R2,P                ESTABLISH PRINT LINE                         
         BRAS  RE,CNTR             CENTER REPORT                                
         USING PRWD,R2                                                          
*                                                                               
         MVC   PRWNID,SRTCMID      COMMERCIAL ID                                
         MVC   PRWTTL,NDNTCMTL     COMMERCIAL TITLE                             
         MVC   PRWNLEN,NDNTCMLN    SPOT LENGTH                                  
         MVC   PRWUSED,NDNTUDTE    USE DATE                                     
         MVC   PRWMKTCD,NDNTNTCD   NETWORK CODE                                 
         MVC   PRWMKTNM,NDNTPGNM   PROGRAM NAME                                 
*                                                                               
         BRAS  RE,SPLAT                                                         
*                                                                               
SRNTOK   DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     SRNTX                                                            
*                                                                               
SRNTERR  DS    0H                  ERROR SKIP RECORD                            
*                                                                               
SRNTERR1 DS    0H                  ERROR SKIP RECORD                            
*                                                                               
         CR    RB,RB                                                            
*                                                                               
SRNTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - PRTR'                                 
***********************************************************************         
*                                                                     *         
*        TRAILER RECORD                                               *         
*                                                                     *         
*NTRY    R3==> UPLOAD RECORD                                          *         
*                                                                     *         
***********************************************************************         
                                                                                
PRTR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   WBYPASS,C'Y'        SKIP IF BYPASSING BATCH                      
         BE    PRTROKAY                                                         
*                                                                               
         USING NDTRD,R3            ESTABLISH UPLOAD TRAILER RECORD              
*                                                                               
         PACK  DUB,NDTRCT          CONVERT RECORD COUNT                         
         TP    DUB                 MAKE SURE IT IS NUMERIC                      
         BNE   PRTRERR1                                                         
*                                                                               
         AP    DUB,=P'2'           INCREMENT FOR HDR AND TRAILER RECS           
         CP    WRECCTR,DUB         MAKE SURE IT MATCHES OUR COUNT               
         BNE   PRTRERR2                                                         
*                                                                               
*        BATCH PASSES ALL CONDITIONS - NOW SORT                                 
*                                                                               
         BRAS  RE,SORT             HANDLE SORTED RECORDS                        
*                                                                               
         BRAS  RE,PUTSREC          ADD ELEMENT TO SYSTEM RECORD                 
*                                                                               
PRTROKAY DS    0H                                                               
*                                                                               
         CR    RB,RB                                                            
         B     PRTRX                                                            
*                                                                               
PRTRERR1 DS    0H                  RECORD COUNT NOT NUMERIC                     
         LAY   R2,ERRTRER1                                                      
         B     PRTRERR                                                          
*                                                                               
PRTRERR2 DS    0H                  RECORD COUNT NOT EQUAL OUR COUNT             
         LAY   R2,ERRTRER2                                                      
         B     PRTRERR                                                          
*                                                                               
PRTRERR  DS    0H                  ERROR SKIP RECORD                            
*                                                                               
         GOTOR SMTPERR,PARMS,(0,0(R2)),(0,0(R3))  E-MAIL ERROR                  
*                                                                               
         LTR   RB,RB                                                            
*                                                                               
         MVI   WBYPASS,C'Y'        BYPASS BATCH                                 
*                                                                               
PRTRX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - SORT'                                 
***********************************************************************         
*                                                                     *         
*        HANDLE RECORDS AFTER THE SORT                                *         
*                                                                     *         
*NTRY                                                                 *         
*                                                                     *         
***********************************************************************         
                                                                                
SORT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        RETRIEVE RECORDS FROM SORT                                             
*                                                                               
SR       DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE HEADLINES                              
         XC    SRTKEYSV,SRTKEYSV   INIT SORT KEY SAVEAREA                       
*                                                                               
         CP    WERRCTR,=P'0'       SKIP IF ERRORS IN FILE                       
         BNE   SRDONE                                                           
*                                                                               
SRLOOP   DS    0H                                                               
*                                                                               
         BRAS  RE,GETSORT          GET FIRST/NEXT RECORD                        
         BZ    SRDONE              DONE IF NO RECORDS                           
*                                                                               
         CLC   SRTKEY(SRTCMID-SRTKEY),SRTKEYSV ON KEY CHANGE                    
         BE    SRNEWN                                                           
*                                                                               
         CLI   WWRKRSW,0           SKIP IF NO WORKER FILE OPEN                  
         BE    *+8                                                              
         BRAS  RE,CLOSWRKR         CLOSE CURRENT WORKER FILE                    
*                                                                               
         MVI   FORCEHED,C'Y'          FORCE HEADLINES                           
         MVC   SRTKEYSV,SRTKEY        SAVE SORTKEY                              
*                                                                               
         BRAS  RE,OPENWRKR         OPEN NEW WORKER FILE                         
*                                                                               
SRNEWN   DS    0H                                                               
*                                                                               
*        MATCH RECORD TYPE TO PROCESSING ROUTINE                                
*                                                                               
         LA    R1,SORTTAB          POINT TO TABLE OF ROUTINES                   
*                                                                               
SRTABLP  DS    0H                                                               
*                                                                               
         CLI   0(R1),X'FF'         DONE AT END OF TABLE                         
         BE    SRTABDN                                                          
*                                                                               
         CLC   SRTRECID,0(R1)      MATCH ON RECORD ID                           
         BE    SRTABFD                                                          
*                                                                               
SRTABCN  DS    0H                                                               
*                                                                               
         LA    R1,8(R1)            NEXT ROUTINE                                 
         B     SRTABLP                                                          
*                                                                               
SRTABFD  DS    0H                  ENTRY IN ROUTINE TABLE FOUND                 
*                                                                               
         ICM   RF,15,4(R1)         GET ROUTINE ADDR FROM TABLE                  
         BASR  RE,RF               PROCESS RECORD                               
*                                                                               
         B     SRCONT                                                           
*                                                                               
SRTABDN  DS    0H                                                               
*                                                                               
         DC    H'0'                UNKNOWN RECORD TYPE                          
*                                                                               
SRCONT   DS    0H                                                               
*                                                                               
         B     SRLOOP              GO READ NEXT RECORD                          
*                                                                               
*        END OF A BATCH                                                         
*                                                                               
SRDONE   DS    0H                                                               
*                                                                               
*        CLOSE CURRENT WORKER FILE                                              
*                                                                               
         CLI   WWRKRSW,X'FF'       IF WORKER FILE OPEN                          
         BNE   *+8                                                              
         BRAS  RE,CLOSWRKR            CLOSE WORKER FILE                         
*                                                                               
*        CLOSE DOWN THE SORT                                                    
*                                                                               
         CLI   WSORTSW,X'FF'       IF SORT ALREADY OPEN                         
         BNE   SRDNSRTN                                                         
*                                                                               
         GOTOR SORTER,DMCB,=C'END'    CLOSE DOWN SORT                           
*                                                                               
         MVI   WSORTSW,0              TURN OFF SWITCH                           
*                                                                               
SRDNSRTN DS    0H                                                               
*                                                                               
SORTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
*        TABLE OF ROUTINES TO PROCESS SORTED RECORD TYPES                       
*                                                                               
*        DC    AL1(RECORD ID),AL3(ROUTINE)                                      
*                                                                               
SORTTAB  DS    0F                                                               
         DC    AL3(NDTWIDQ),AL1(0),AL4(SRTW) TV WILD SPOTS                      
         DC    AL3(NDRWIDQ),AL1(0),AL4(SRRW) RADIO WILD SPOTS                   
         DC    AL3(NDLCIDQ),AL1(0),AL4(SRLC) LOCAL CABLE                        
         DC    AL3(NDCBIDQ),AL1(0),AL4(SRCB) CABLE SYSTEMS                      
         DC    AL3(NDNTIDQ),AL1(0),AL4(SRNT) NETWORK                            
*                                                                               
         DC    X'FF'               EOT                                          
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - PUTSORT'                              
***********************************************************************         
*                                                                     *         
*        ADD RECORD TO SORT                                           *         
*                                                                     *         
***********************************************************************         
                                                                                
PUTSORT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   TRACOPT,C'Y'        SKIP IF NOT TRACING                          
         BNE   PUTSORT1                                                         
*                                                                               
         MVC   P(6),=C'PUTSRT'     SET ACTION CODE                              
         BRAS  RE,SPLAT                                                         
*                                                                               
         GOTO1 HEXOUT,DMCB,SRTKEY,P,56  PRINT KEY                               
         GOTO1 HEXOUT,DMCB,SRTKEY+56,P2,40  PRINT KEY                           
*                                                                               
         MVC   P3+5(100),SRTDATA        PRINT DATA                              
         MVC   P4+5(100),SRTDATA+100                                            
*                                                                               
         BRAS  RE,SPLAT                                                         
*                                                                               
PUTSORT1 DS    0H                                                               
*                                                                               
         GOTOR SORTER,DMCB,=C'PUT',SRTREC    PUT TO SORT                        
*                                                                               
PUTSORTX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - GETSORT'                              
***********************************************************************         
*                                                                     *         
*        GET RECORD FROM SORT                                         *         
*                                                                     *         
***********************************************************************         
                                                                                
GETSORT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTOR SORTER,DMCB,=C'GET',SRTREC    GET FROM SORT                      
*                                                                               
         ICM   R2,15,DMCB+4        GET A(RECORD)                                
         BZ    GETSORTX            NO MORE RECORDS                              
*                                                                               
         MVC   SRTKEY(SRTHDRLN),0(R2)   COPY SORT HEADER                        
         MVC   SRTDATA,SRTHDRLN(R2)     COPY SORT DATA                          
*                                                                               
         CLI   TRACOPT,C'Y'        SKIP IF NOT TRACING                          
         BNE   GETSORT1                                                         
*                                                                               
         MVC   P(6),=C'GETSRT'     SET ACTION CODE                              
         BRAS  RE,SPLAT                                                         
*                                                                               
         GOTO1 HEXOUT,DMCB,SRTKEY,P,56  PRINT KEY                               
*                                                                               
         MVC   P2+5(100),SRTDATA        PRINT DATA                              
         MVC   P3+5(100),SRTDATA+100                                            
*                                                                               
         BRAS  RE,SPLAT                                                         
*                                                                               
GETSORT1 DS    0H                                                               
*                                                                               
         LTR   RB,RB               SET CC NEQ                                   
*                                                                               
GETSORTX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - PUTSREC'                              
***********************************************************************         
*                                                                     *         
*        UPDATE SYSTEM RECORD FOR BATCH                               *         
*                                                                     *         
***********************************************************************         
                                                                                
PUTSREC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        BUILD SYSTEM RECORD FILENAME ELEMENT                                   
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT AS FILENAME ELM                 
         LA    R6,ELEMENT                                                       
         USING TAFID,R6                                                         
*                                                                               
         MVI   TAFIEL,TAFIELQ      SET ELEMENT ID                               
         MVC   TAFIDATE,WHDDATE    SET DATE                                     
         MVC   TAFITIME,WHDTIME    SET TIME                                     
*                                                                               
*        FIND LENGTH OF FILE NAME                                               
*                                                                               
         LA    R1,WHDFLNM+L'WHDFLNM-1 POINT TO END OF FILE NAME                 
         LA    RF,L'WHDFLNM        SET LOOP COUNTER                             
*                                                                               
         CLI   0(R1),C' '          FIND LAST NON-SPACE                          
         BH    *+12                                                             
         BCTR  R1,0                BACK UP A POSITION                           
         BCT   RF,*-10             BACKUP A POSITION                            
         DC    H'0'                SHOULD NOT HAPPEN                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAFIFILE(0),WHDFLNM SET FILE NAME IN ELEMENT                     
*                                                                               
         AHI   RF,TAFILNQ+1        ELEMENT LENGTH                               
         STC   RF,TAFILEN          SET ELEMENT LENGTH                           
*                                                                               
         CLI   TRACOPT,C'Y'        IF TRACING                                   
         BNE   PSR10                                                            
*                                                                               
         BRAS  RE,SPLAT                                                         
         MVC   P(7),=C'PUTSELM'    SET ACTION CODE                              
         BRAS  RE,SPLAT                                                         
*                                                                               
         LLC   RF,ELEMENT+1        GET ELEMENT LENGTH                           
*                                                                               
         GOTO1 HEXOUT,DMCB,ELEMENT,P,(RF) PRINT ELEMENT                         
         BRAS  RE,SPLAT                                                         
*                                                                               
PSR10    DS    0H                                                               
*                                                                               
*        FIND SYSTEM RECORD FOR ELEMENT                                         
*                                                                               
         MVC   KEY,WFLKYSV         SET KEY OF LAST RECORD READ                  
         USING TLSYKEY,R4                                                       
         LA    R4,KEY                                                           
*                                                                               
         OC    KEY,KEY             IF LAST KEY NOT KNOWN                        
         BNZ   PSROLD                 ADD NEW RECORD                            
*                                                                               
*        BUILD KEY FOR NEW RECORD                                               
*                                                                               
         MVI   TLSYCD,TLSYCDQ      SET RECORD CODE                              
         MVI   TLSYTYPE,TLSYNCFN   SET TYPE AS NON-CLARUS FILENAMES             
         MVC   TLSYNCDT,WHDDATEP   SET FILE DATE                                
         MVC   TLSYNUID,WHDUID     SET USERID                                   
*                                                                               
         MVC   WFLKYSV,KEY         SAVE LATEST KEY                              
*                                                                               
         MVC   AIO,AIO1            BUILD REC IN AIO1                            
*                                                                               
         L     R4,AIO              POINT TO RECORD BUILD AREA                   
         MVC   TLSYKEY,KEY         SET KEY                                      
         MVC   TLSYLEN,=AL2(TLSYELEM-TLSYKEY+1) SET BASIC LENGTH                
         MVI   TLSYELEM,0          TRAILING NULLS                               
*                                                                               
         GOTOR ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
         CLI   TRACOPT,C'Y'        IF TRACING                                   
         BNE   PSR20                                                            
*                                                                               
         MVC   P,SPACES                                                         
         BRAS  RE,SPLAT                                                         
         MVC   P(7),=C'ADDSREC'    SET ACTION CODE                              
         BRAS  RE,SPLAT                                                         
*                                                                               
         GOTO1 HEXOUT,DMCB,TLSYKEY,P,50   PRINT KEY                             
         BRAS  RE,SPLAT                                                         
*                                                                               
PSR20    DS    0H                                                               
*                                                                               
         GOTOR ADDREC              ADD RECORD TO FILE                           
*                                                                               
         B     PUTSRECX            ALL DONE                                     
*                                                                               
PSROLD   DS    0H                                                               
*                                                                               
         GOTOR HIGH                READ FOR RECORD                              
*                                                                               
         MVC   AIO,AIO1            USE AIO1                                     
*                                                                               
         GOTOR GETREC              READ IN RECORD                               
*                                                                               
         GOTOR ADDELEM             ADD FILE NAME ELEMENT TO RECORD              
*                                                                               
         CLI   ERROR,TOOLONG       SKIP IF NO ROOM FOR ELEMENT                  
         BE    PSRNXT                                                           
*                                                                               
         CLI   TRACOPT,C'Y'        IF TRACING                                   
         BNE   PSR30                                                            
*                                                                               
         MVC   P,SPACES                                                         
         BRAS  RE,SPLAT                                                         
         MVC   P(7),=C'PUTSREC'    SET ACTION CODE                              
         BRAS  RE,SPLAT                                                         
*                                                                               
         GOTO1 HEXOUT,DMCB,TLSYKEY,P,50   PRINT KEY                             
         BRAS  RE,SPLAT                                                         
*                                                                               
PSR30    DS    0H                                                               
*                                                                               
         GOTOR PUTREC              RE-WRITE RECORD TO FILE                      
*                                                                               
         B     PUTSRECX                                                         
*                                                                               
*        ADD ELEMENT TO SAME KEY PLUS ONE IN SEQ NUMBER                         
*                                                                               
PSRNXT   DS    0H                                                               
*                                                                               
         L     R4,AIO               POINT TO CURRENT RECORD                     
*                                                                               
         LLC   RF,TLSYSEQ          BUMP SEQUENCE NMBER BY ONE                   
         AHI   RF,1                                                             
         STC   RF,TLSYSEQ                                                       
*                                                                               
         MVC   TLSYLEN,=AL2(TLSYELEM-TLSYKEY+1) SET BASIC LENGTH                
         MVI   TLSYELEM,0          TRAILING NULLS                               
*                                                                               
         GOTOR ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
         CLI   TRACOPT,C'Y'        IF TRACING                                   
         BNE   PSR40                                                            
*                                                                               
         MVC   P,SPACES                                                         
         BRAS  RE,SPLAT                                                         
         MVC   P(7),=C'NXTSREC'    SET ACTION CODE                              
         BRAS  RE,SPLAT                                                         
*                                                                               
         GOTO1 HEXOUT,DMCB,TLSYKEY,P,50   PRINT KEY                             
         BRAS  RE,SPLAT                                                         
*                                                                               
PSR40    DS    0H                                                               
*                                                                               
         GOTOR ADDREC              ADD RECORD TO FILE                           
*                                                                               
         B     PUTSRECX                                                         
*                                                                               
PUTSRECX DS    0H                  NO PRIOR UPLOAD - PROCESS                    
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - INIT'                                 
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
                                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   TRACOPT,C'Y'        SKIP IF TRACING                              
         BE    INIT1                                                            
*                                                                               
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK         SET A(HEADHOOK)                              
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS            SET A(SPECS)                                 
*                                                                               
         MVI   FORCEHED,C'Y'      FORCE HEADLINES                               
*                                                                               
INIT1    DS    0H                                                               
*                                                                               
         CLI   WINITSW,X'FF'       SKIP IF INIT DONE                            
         BE    INITX                                                            
*                                                                               
         MVI   WINITSW,X'FF'       INDICATE INIT DONE                           
*                                                                               
         XC    WAGY,WAGY           INIT AGENCY SAVEAREA                         
         XC    WAGYNAME,WAGYNAME   INIT AGENCY NAME SAVEAREA                    
*                                                                               
         GOTOR DATCON,DMCB,(5,0),(1,WTODAY) TODAY AS PWOS                       
*                                                                               
*        INITIALIZE AGENCY TABLE                                                
*                                                                               
         LA    R1,TABAGYTB         POINT TO AGENCY TABLE                        
         LHI   RF,TAGYLQ           LENGTH OF TABLE ENTRY                        
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)       INIT FIRST ENTRY IN TABLE                    
*                                                                               
         MVI   0(R1),X'FF'         SET END OF TABL                              
*                                                                               
*                                                                               
         ZAP   WRECCTR,=P'0'       INIT RECORD COUNTER                          
         ZAP   WERRCTR,=P'0'       INIT ERROR  COUNTER                          
*                                                                               
*        OPEN UPLOAD TAPE                                                       
*                                                                               
         LA    R2,UPLDTAPE                                                      
         OPEN  ((R2),INPUT)                                                     
         LTR   RF,RF               NO ERRORS ALLOWED                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INITX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0D                  ALIGNMENT                                    
UPLDTAPE DCB   DDNAME=UPLDTAPE,DSORG=PS,MACRF=GM,EODAD=PRDONE                   
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - DTEVAL'                               
***********************************************************************         
*                                                                     *         
*        VALIDATE DATE OF FORMAT YYYYMMDD                             *         
*                                                                     *         
*NTRY    P1    A(INPUT  DATE)  YYYYMMDD CH                            *         
*        P2    A(OUTPUT DATE)  YMD PWOS                               *         
*                                                                     *         
***********************************************************************         
                                                                                
         DS    0D                  ALIGNMENT                                    
DTEVAL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LM    R2,R3,0(R1)         SAVE PARAMETER LIST                          
******                                                                          
         MVC   DUB,0(R2)           VERIFY NUMERIC INPUT                         
         NC    DUB,=8C'0'                                                       
         CLC   DUB,=8C'0'                                                       
         BNE   DTEVALE1               NOT NUMERIC                               
******                                                                          
******   XC    WORK,WORK           CLEAR WORKAREA                               
******   GOTOR DATCON,DMCB,(9,0(R2)),(17,WORK)  MMDD/YY                         
******                                                                          
******   GOTOR DATVAL,DMCB,WORK,WORK+10 VALIDATE AS DATE                        
******   CLC   WORK+10(6),=C'000000'  TEST FOR ERRORS                           
******   BE    DTEVALE2               INVALID DATE                              
******                                                                          
         GOTOR DATVAL,DMCB,(0,0(R2)),WORK  VALIDATE DATE                        
         CLC   WORK(6),=C'000000'     TEST FOR ERRORS                           
         BE    DTEVALE2               INVALID DATE                              
*                                                                               
         GOTOR DATCON,DMCB,WORK,(20,WORK+8)  SAVE AS PWOS                       
         CLC   0(8,R2),WORK+8         MAKE SURE FORMAT WAS YYYYMMDD             
         BNE   DTEVALE2                                                         
*                                                                               
         GOTOR DATCON,DMCB,WORK,(1,0(R3))  SAVE AS PWOS                         
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     DTEVALX                                                          
*                                                                               
*        DATE ERRORS                                                            
*                                                                               
DTEVALE1 DS    0H                  DATE NOT NUMERIC                             
*                                                                               
DTEVALE2 DS    0H                  DATE NOT VALID                               
*                                                                               
         XC    0(3,R3),0(R3)       CLEAR DATE SAVEAREA                          
         LTR   RB,RB               SET NEQ CC                                   
*                                                                               
DTEVALX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - TMEVAL'                               
***********************************************************************         
*                                                                     *         
*        VALIDATE TIME FORMAT OF HH:MM:SS                             *         
*                                                                     *         
*NTRY    P1    A(INPUT  TIME)  HH:MM:SS                               *         
*        P2    A(OUTPUT TIME)  HMS PWOS                               *         
*                                                                     *         
***********************************************************************         
                                                                                
TMEVAL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LM    R2,R3,0(R1)         SAVE PARAMETER LIST                          
*                                                                               
         XC    WHDTIME,WHDTIME     CLEAR HEADER TIME SAVEAREA                   
*                                                                               
         CLI   2(R2),C':'          CHECK FOR DELIMITERS                         
         BNE   TMEVALE1                                                         
         CLI   5(R2),C':'                                                       
         BNE   TMEVALE1                                                         
*                                                                               
*        VALIDATE HOURS ARE 0 TO 24                                             
*                                                                               
         MVC   DUB(2),0(R2)        VERIFY NUMERIC INPUT                         
         NC    DUB(2),=8C'0'                                                    
         CLC   DUB(2),=8C'0'                                                    
         BNE   TMEVALE2               NOT NUMERIC                               
*                                                                               
         PACK  DUB,0(2,R2)         VERIFY WITHIN LIMITS                         
*                                                                               
         CP    DUB,=P'0'           MUST BE 0 TO 24 HRS                          
         BL    TMEVALE3                                                         
         CP    DUB,=P'24'                                                       
         BH    TMEVALE3                                                         
*                                                                               
         SRP   DUB,1,0             MOVE ONE NYBBLE LEFT                         
*                                                                               
         MVC   0(1,R3),DUB+6       SAVE HOURS                                   
*                                                                               
*        VALIDATE MINUTES ARE 0 TO 60                                           
*                                                                               
         MVC   DUB(2),3(R2)        VERIFY NUMERIC INPUT                         
         NC    DUB(2),=8C'0'                                                    
         CLC   DUB(2),=8C'0'                                                    
         BNE   TMEVALE2               NOT NUMERIC                               
*                                                                               
         PACK  DUB,3(2,R2)         VERIFY WITHIN LIMITS                         
*                                                                               
         CP    DUB,=P'0'           MUST BE 0 TO 60 MINUTES                      
         BL    TMEVALE3                                                         
         CP    DUB,=P'60'                                                       
         BH    TMEVALE3                                                         
*                                                                               
         SRP   DUB,1,0             MOVE ONE NYBBLE LEFT                         
*                                                                               
         MVC   1(1,R3),DUB+6       SAVE MINUTES                                 
*                                                                               
*        VALIDATE SECONDS ARE 0 TO 60                                           
*                                                                               
         MVC   DUB(2),6(R2)        VERIFY NUMERIC INPUT                         
         NC    DUB(2),=8C'0'                                                    
         CLC   DUB(2),=8C'0'                                                    
         BNE   TMEVALE2               NOT NUMERIC                               
*                                                                               
         PACK  DUB,6(2,R2)         VERIFY WITHIN LIMITS                         
*                                                                               
         CP    DUB,=P'0'           MUST BE 0 TO 60 SECS                         
         BL    TMEVALE3                                                         
         CP    DUB,=P'60'                                                       
         BH    TMEVALE3                                                         
*                                                                               
         SRP   DUB,1,0             MOVE ONE NYBBLE LEFT                         
*                                                                               
         MVC   1(2,R3),DUB+6       SAVE SECONDS                                 
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     TMEVALX                                                          
*                                                                               
*        TIME ERRORS                                                            
*                                                                               
TMEVALE1 DS    0H                  INVALID FORMAT                               
*                                                                               
TMEVALE2 DS    0H                  TIME NOT NUMERIC                             
*                                                                               
TMEVALE3 DS    0H                  TIME NOT VALID                               
*                                                                               
         XC    0(3,R3),0(R3)       CLEAR DATE SAVEAREA                          
         LTR   RB,RB               SET NEQ CC                                   
*                                                                               
TMEVALX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - AGYVAL'                               
***********************************************************************         
*                                                                     *         
*        VALIDATE AGENCY CODE                                         *         
*                                                                     *         
*NTRY    P1    A(AGENCY CODE)                                         *         
*                                                                     *         
***********************************************************************         
                                                                                
AGYVAL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,0(R1)            POINT TO AGENCY CODE                         
*                                                                               
*        TEST IF AGENCY ALREADY IN TABLE                                        
*                                                                               
         LA    R2,TABAGYTB         ESTABLISH AGENCY TABLE                       
         USING TABAGYD,R2                                                       
*                                                                               
AGYVALLP DS    0H                                                               
*                                                                               
         CLI   TAGYAGY,X'FF'       TEST FOR EOT                                 
         BE    AGYVALDN                                                         
*                                                                               
         CLC   TAGYAGY,0(R3)       MATCH ON AGENCY CODE                         
         BE    AGYVALFD                                                         
*                                                                               
AGYVALCN DS    0H                                                               
         LA    R2,TAGYLQ(R2)       BUMP TO NEXT IN TABLE                        
         B     AGYVALLP                                                         
*                                                                               
AGYVALDN DS    0H                  NOT IN TABLE                                 
*                                                                               
         XC    WAGY,WAGY           INIT AGENCY CODE                             
         XC    WAGYNAME,WAGYNAME   INIT AGENCY NAME                             
*                                                                               
*        FIND AGENCY RECORD ON FILE                                             
*                                                                               
         XC    KEY,KEY             ESTABLISH AS AGENCY KEY                      
         LA    R4,KEY                                                           
         USING TLAYD,R4                                                         
*                                                                               
         MVI   TLAYCD,TLAYCDQ      SET AGENCY RECORD ID                         
         MVC   TLAYAGY,0(R3)       SET AGENCY CODE                              
*                                                                               
         GOTOR HIGH                READ DIRECTORY                               
*                                                                               
         CLC   TLAYKEY,KEYSAVE     MUST FIND RECORD                             
         BNE   AGYVALER                                                         
*                                                                               
         MVC   AIO,AIO2            SET I/O AREA                                 
*                                                                               
         GOTOR GETREC              READ IN AGENCY RECORD                        
*                                                                               
         MVC   AIO,AIO1            RESET I/O AREA                               
*                                                                               
         MVC   TAGYAGY,TLAYAGY     SAVE AGENCY CODE IN TABLE                    
         XC    TAGYNAME,TAGYNAME   INIT AGENCY NAME                             
*                                                                               
         L     R6,AIO2             POINT TO FOUND RECORD                        
         MVI   ELCODE,TANAELQ      SET FOR NAME ELEMENT                         
         GOTOR GETEL               FIND ELEMENT                                 
         BNE   AGYVAL30            NO NAME ELEMENT                              
*                                                                               
         USING TANAD,R6            ESTABLISH NAME ELEMENT                       
*                                                                               
         LLC   RF,TANALEN          ELEMENT LENGTH                               
         SHI   RF,TANALNQ          LESS HEADER LENGTH                           
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAGYNAME(0),TANANAME SAVE AGENCY NAME                            
*                                                                               
         LA    R2,TAGYLQ(R2)       BUMP TO NEXT ELEMENT IN TABLE                
*                                                                               
         XC    TAGYENT(TAGYLQ),TAGYENT INIT ELEMENT                             
         MVI   TAGYENT,X'FF'       RESET EOT                                    
*                                                                               
         SHI   R2,TAGYLQ           BACK UP TO LAST ELM                          
*                                                                               
         B     AGYVAL90                                                         
*                                                                               
AGYVAL30 DS    0H                                                               
*                                                                               
         L     R6,AIO2             POINT TO FOUND RECORD                        
         MVI   ELCODE,TAFNELQ      SET FOR NAME ELEMENT                         
         GOTOR GETEL               FIND ELEMENT                                 
*                                                                               
AGYVAL40 DS    0H                                                               
         BNE   AGYVAL60            NO NAME ELEMENT                              
*                                                                               
         USING TAFND,R6            ESTABLISH NAME ELEMENT                       
*                                                                               
         CLI   TAFNTYPE,TAFNTAGY   MUST BE AGENCY NAME ELEMENT                  
         BE    AGYVAL50                                                         
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT ELEMENT                            
         B     AGYVAL40                                                         
*                                                                               
AGYVAL50 DS    0H                  NAME ELEMENT FOUND                           
*                                                                               
         LLC   RF,TAFNLEN          ELEMENT LENGTH                               
         SHI   RF,TAFNLNQ          LESS HEADER LENGTH                           
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAGYNAME(0),TAFNNAME SAVE AGENCY NAME                            
*                                                                               
AGYVAL60 DS    0H                                                               
*                                                                               
         LA    R2,TAGYLQ(R2)       BUMP TO NEXT ELEMENT IN TABLE                
*                                                                               
         XC    TAGYENT(TAGYLQ),TAGYENT INIT ELEMENT                             
         MVI   TAGYENT,X'FF'       RESET EOT                                    
*                                                                               
         SHI   R2,TAGYLQ           BACK UP TO LAST ELM                          
*                                                                               
AGYVAL90 DS    0H                                                               
*                                                                               
AGYVALFD DS    0H                                                               
*                                                                               
         MVC   WAGY,TAGYAGY        SET AGENCY CODE                              
         MVC   WAGYNAME,TAGYNAME   SET AGENCY NAME                              
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     AGYVALX                                                          
*                                                                               
*        AGENCY CODE NOT ON FILE                                                
*                                                                               
AGYVALER DS    0H                                                               
*                                                                               
         XC    WAGY,WAGY           CLEAR AGENCY CODE SAVEAREA                   
         XC    WAGYNAME,WAGYNAME   CLEAR AGENCY NAME SAVEAREA                   
*                                                                               
         LTR   RB,RB               SET NEQ CC                                   
*                                                                               
AGYVALX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - SPLAT'                                
***********************************************************************         
*                                                                     *         
*        PRINT LINES                                                  *         
*                                                                     *         
***********************************************************************         
                                                                                
SPLAT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BRAS  RE,MYCLEAR                                                       
*                                                                               
SPLATX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - MYCLEAR'                              
***********************************************************************         
*                                                                     *         
*        CLEAR LOCAL PRINT LINES                                      *         
*                                                                     *         
***********************************************************************         
                                                                                
MYCLEAR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   P,C' '                                                           
         MVC   P+1(131),P                                                       
         MVC   P2,P                                                             
         MVC   P3,P                                                             
         MVC   P4,P                                                             
*                                                                               
MYCLEARX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - AGYVAL'                               
***********************************************************************         
*                                                                     *         
*        VALIDATE COMMERCIAL ID                                       *         
*                                                                     *         
*NTRY    P1    UPLOADED COMMERCIAL CODE                               *         
*                                                                     *         
*EXIT          TGCID - COMMERCIAL ID                                  *         
*              TGCOM - INTERIOR COMMERCIAL ID                         *         
*                                                                     *         
***********************************************************************         
                                                                                
COMVAL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,0(R1)            POINT TO COMMERCIAL ID                       
*                                                                               
         MVC   TGADID,0(R3)        SET COMMERCIAL ID                            
         OC    TGADID,SPACES       SPACE FILL                                   
         MVC   TGAGY,WAGY          SET UPLOADING AGENCY                         
*                                                                               
         XC    TGCID,TGCID         INIT FIELDS                                  
         XC    TGNID,TGNID                                                      
*                                                                               
         MVI   WMTCH,0             ASSUME COMMERCIAL UNMATCHED                  
         MVC   AIO,AIO2            SET I/O AREA                                 
*                                                                               
         BRAS  RE,CHKNID           MATCH TO TALENT COMMERCIAL                   
         BNE   COMVALX             COMMERCIAL NOT MATCHED                       
*                                                                               
         MVI   WMTCH,1             COMMERCIAL MATCHED                           
*                                                                               
         L     R4,AIO2             POINT TO COMMERCIAL RECORD                   
         USING TLCOD,R4            ESTABLISH RECORD                             
*                                                                               
         MVC   TGCOM,TLCOCOM       SAVE INTERNAL COMMERCIAL ID                  
*                                                                               
         L     R6,AIO2             POINT TO FOUND RECORD                        
         MVI   ELCODE,TACOELQ      SET FOR COMMERCIAL DETAILS ELM               
         BRAS  RE,GETEL            FIND ELEMENT                                 
*                                                                               
         BNE   CMV20               NO NAME ELEMENT                              
*                                                                               
         USING TACOD,R6            ESTABLISH NAME ELEMENT                       
*                                                                               
         MVC   TGCID,TACOCID       SAVE COMMERCIAL ID                           
*                                                                               
CMV20    DS    0H                                                               
*                                                                               
*        FIND COMMERCIAL TITLE                                                  
*                                                                               
         L     R6,AIO2             POINT TO FOUND RECORD                        
         MVI   ELCODE,TACOELQ      SET FOR COMMERCIAL DETAILS ELM               
         BRAS  RE,GETEL            FIND ELEMENT                                 
*                                                                               
CMVTTLLP DS    0H                                                               
*                                                                               
         BNE   CMVTTLDN                                                         
*                                                                               
         USING TAFND,R6            ESTABLISH FRRE-FOEM NAME ELEMENT             
*                                                                               
         CLI   TAFNTYPE,TAFNTTTL   LOOKING FOR TITLE ELEMENT                    
         BE    CMVTTLFD                                                         
*                                                                               
CMVTTLCN DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         B     CMVTTLLP                                                         
*                                                                               
CMVTTLFD DS    0H                  TITLE ELEMENT FOUND                          
*                                                                               
         LLC   RF,TAFNLEN          GET ELEMENT LENGTH                           
         SHI   RF,TAFNLNQ          LESS HEADER LENGTH                           
*                                                                               
         MVC   WCOMTTL,SPACES      INIT COMMERCIAL TITLE                        
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WCOMTTL(0),TAFNNAME                                              
*                                                                               
CMVTTLDN DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     COMVALX                                                          
*                                                                               
COMVALX  DS    0H                                                               
         MVC   AIO,AIO1            RESET I/O AREA                               
         XIT1                                                                   
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
       ++INCLUDE TACHKNID                                                       
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
                                                                                
XIT      XIT1                                                                   
                                                                                
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - HOOK'                                 
***********************************************************************         
*                                                                     *         
*              HEADLINE/BOX ROUTINES (HEADHOOK)                       *         
*                                                                     *         
***********************************************************************         
                                                                                
HOOK     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LAY   R5,SRTDATA          POINT TO LATEST SORT RECORD                  
         USING NDTWD,R5            ESTABLISH AS TV WILD SPOT                    
*                                                                               
         BAS   RE,INTBOX           INITIALIZE BOXES                             
*                                                                               
         MVC   H3+9(L'WAGY),WAGY    SET AGENCY CODE AND NAME                    
         MVC   H3+17(L'WAGYNAME),WAGYNAME                                       
*                                                                               
         MVC   H3+116(L'WAGY),WHDMDCO                                           
         MVC   H4+98(33),WAGYNAME                                               
*                                                                               
         MVC   H3+58(12),SPACES    SET TITLE FOR REPORT                         
*                                                                               
         CLI   RCSUBPRG,28         IF UNMATCHED REPORT                          
         BNE   *+14                                                             
         MVC   H3+58(12),=CL12' UNMATCHED'                                      
         B     HOOK06                                                           
*                                                                               
         CLI   RCSUBPRG,29         IF MATCHED REPORT                            
         BNE   *+14                                                             
         MVC   H3+58(12),=CL12'  MATCHED'                                       
         B     HOOK06                                                           
*                                                                               
         CLI   RCSUBPRG,60         IF SUMMARY REPORT                            
         BL    HOOK05                                                           
*                                                                               
         CLI   RCSUBPRG,61                                                      
         BH    HOOK05                                                           
*                                                                               
         MVC   H3+61(7),=C'SUMMARY'                                             
*                                                                               
         B     HOOKX                                                            
*                                                                               
HOOK05   CLI   RCSUBPRG,70         IF SUMMARY REPORT                            
         BL    HOOK06                                                           
*                                                                               
         CLI   RCSUBPRG,71                                                      
         BH    HOOK06                                                           
*                                                                               
         MVC   H3+58(7),=C'SUMMARY'                                             
         B     HOOKX                                                            
*                                                                               
HOOK06   LA    R2,P                                                             
         USING PRNTD,R2                                                         
*                                                                               
         BAS   RE,CNTR                                                          
*                                                                               
         CLC   PRTNID,SPACES                                                    
         BNE   HOOK10                                                           
*                                                                               
******   L     R4,ANEWHOLD                                                      
         USING TLNXD,R4                                                         
*                                                                               
         CLI   TLNXCD,TLNXCDQ      IF TRANSFER RECORD                           
         BNE   HOOK10                                                           
*                                                                               
         CLI   RECNUM,SX                                                        
         BE    *+14                                                             
         OC    USECNT,USECNT       AND IF USES TO PRINT                         
         BZ    HOOK10                                                           
*                                                                               
         MVC   PRTNID,TLNXNID      SET NETWORK ID (CONTINUED)                   
*                                                                               
         TM    TLNXSTAT,TLNXPACK   IF PACKED, UNPACK                            
         BZ    HOOK08                                                           
*                                                                               
         XC    WORK,WORK                                                        
*****    GOTO1 ATRPACK,DMCB,(C'U',TLNXNID),WORK                                 
*                                                                               
         MVC   PRTNID(12),WORK                                                  
HOOK08   MVC   PRTCONT,=C'(CONTINUED)'                                          
*                                                                               
HOOK10   MVC   H4+9(L'SRTCLCD),SRTCLCD   TALENT OR NETWORK CLIENT CODE          
         MVC   H4+17(L'NDTWCLNM),NDTWCLNM TALENT OR NETWORK CLIENT NAME         
*                                                                               
         MVC   H5+9(L'SRTPRCD),SRTPRCD   TALENT OR NETWORK PRODUCT CODE         
         MVC   H5+17(L'NDTWPRNM),NDTWPRNM TALENT OR NETWORK PROD NAME           
*                                                                               
         MVC   H6+9(L'SRTMDCD),SRTMDCD    MEDIA CODE                            
*                                                                               
         MVC   H6+17(16),=CL16'SPOT TV'   ASSUME MEDIA IS TV                    
*                                                                               
         CLI   SRTMDCD,C'R'        CHANGE TITLE IF RADIO                        
         BNE   *+10                                                             
         MVC   H6+17(16),=CL16'SPOT RADIO'                                      
*                                                                               
*****    MVC   H7+9(L'SRTUSE),SRTUSE      USE CODE                              
*                                                                               
*******  CLI   REPTYPE,SRTTYPU                                                  
*******  BE    HOOK20                                                           
*******  MVC   H3+61(7),=C'MATCHED'                                             
*******  B     HOOKX                                                            
*******                                                                         
*HOOK20  MVC   H3+60(9),=C'UNMATCHED'                                           
*                                                                               
HOOKX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - INTBOX'                               
***********************************************************************         
*                                                                     *         
*        INTERIOR BOXES                                               *         
*                                                                     *         
***********************************************************************         
                                                                                
INTBOX   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXYORN,C'N'                                                     
*                                                                               
         CLI   RCSUBPRG,60         IF SUMMARY REPORT                            
         BNL   INTBOXX             DON'T WANT ANY BOXES                         
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVI   BOXWT,1                                                          
         MVI   BOXROWS+8,C'T'                                                   
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+60,C'B'                                                  
*                                                                               
*        UNMATCHED WILDSPOTS                                                    
*                                                                               
         CLI   RCSUBPRG,28        IF UNMATCHED WILDSPOTS                        
         BNE   INTB28X                                                          
*                                                                               
         LA    R2,BOXCOLS                                                       
         BAS   RE,CNTR             CENTER THE REPORT,R2 SET ON XIT              
         USING PRWD,R2             ESTABLISH PRINT LINE                         
*                                                                               
         MVI   PRWBL,C'L'                                                       
         MVI   PRWBC1,C'C'                                                      
         MVI   PRWBC2,C'C'                                                      
         MVI   PRWBC3,C'C'                                                      
         MVI   PRWBRC,C'R'                                                      
*                                                                               
         B     INTBOXX                                                          
*                                                                               
INTB28X  DS    0H                                                               
*                                                                               
         LA    R2,BOXCOLS                                                       
         BAS   RE,CNTR             CENTER THE REPORT,R2 SET ON XIT              
         USING PRNTD,R2                                                         
         MVI   BL,C'L'                                                          
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
*****    CLI   REPTYPE,SRTTYPU                                                  
*****    BE    INTBOX5                                                          
         MVI   BC3,C'C'                                                         
         MVI   BC4,C'C'                                                         
         MVI   BC5,C'C'                                                         
*******  BAS   RE,CBLCHK                                                        
*******  BE    INTBOX0C                                                         
*******  BAS   RE,WSPCHK                                                        
*******  BE    INTBOX0D                                                         
*******  BAS   RE,LCBCHK                                                        
*******  BE    INTBOX0D                                                         
         MVI   BC6,C'C'                                                         
         MVI   BR,C'R'                                                          
         B     INTBOXX                                                          
INTBOX0C MVI   BC6C,C'C'                                                        
         MVI   BRC,C'R'                                                         
         B     INTBOXX                                                          
INTBOX0D MVI   BC6C,C'R'                                                        
         B     INTBOXX                                                          
*******                                                                         
*                                                                               
INTBOX5  MVI   UBC3,C'C'                                                        
*******  BAS   RE,CBLCHK                                                        
*******  BE    INTBOX5C                                                         
*******  BAS   RE,WSPCHK                                                        
*******  BE    INTBOX5C                                                         
*******  BAS   RE,LCBCHK                                                        
*******  BE    INTBOX5C                                                         
         MVI   UBC4,C'C'                                                        
         MVI   UBC5,C'C'                                                        
         MVI   UBR,C'R'                                                         
         B     INTBOXX                                                          
INTBOX5C MVI   UBC4C,C'C'                                                       
         MVI   UBC6C,C'C'                                                       
         MVI   UBRC,C'R'                                                        
         B     INTBOXX                                                          
*                                                                               
INTBOXX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R2,R4                                                            
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - BXBOT'                                
***********************************************************************         
*                                                                     *         
*              ROUTINE TO PRINT MIDDLE LINE                           *         
*                                                                     *         
***********************************************************************         
                                                                                
BXBOT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
*                                                                               
         MVC   BOXROWS,SPACES                                                   
*                                                                               
         ZIC   RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'M'                                                       
*                                                                               
         MVI   BOXINIT,0                                                        
*                                                                               
         BRAS  RE,SPLAT                                                         
*                                                                               
BXBOTX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - CNTR'                                 
***********************************************************************         
*                                                                     *         
*              ROUTINE TO CENTER REPORT                               *         
*                                                                     *         
***********************************************************************         
         SPACE                                                                  
CNTR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   RCSUBPRG,28         IF UNMATCHED WILDSPOTS                       
         BNE   *+14                                                             
         LA    R1,DSP28            R1=DISPLACEMENT FROM START OF LINE           
         B     CNTRX                                                            
*                                                                               
CNTRX    AR    R2,R1                                                            
         XIT1  REGS=(R2)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
DSP28    EQU   (132-(PRWBRC-PRWBL+1))/2  DISP TO START OF REPORT                
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - MYSPECS'                              
***********************************************************************         
*                                                                     *         
*              SPECS                                                  *         
*                                                                     *         
***********************************************************************         
                                                                                
MYSPECS  DS    0H                                                               
         SPROG 10,20,25,26,27,28,29,30,60,61,70,71                              
         SSPEC H1,2,RUN                                                         
         SSPEC H1,99,REPORT                                                     
         SSPEC H1,119,PAGE                                                      
         SSPEC H2,99,REQUESTOR                                                  
         SSPEC H3,2,C'AGENCY'                                                   
         SSPEC H3,99,C'TRANSFERRED FROM:'                                       
*                                                                               
         SPROG 10,20,25,26,27,28,29,30                                          
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H5,2,C'PRODUCT'                                                  
         SSPEC H6,2,C'MEDIA'                                                    
         SSPEC H7,2,C'USE'                                                      
*                                                                               
         SPROG 10,20,25,26,28,60                                                
         SSPEC H1,54,C'NON-D|S CLARUS INTERFACE'                                
         SSPEC H2,54,24X'BF'                                                    
*                                                                               
         SPROG 27,29,30,61,71                                                   
         SSPEC H1,59,C'SPOT INTERFACE'                                          
         SSPEC H2,59,14X'BF'                                                    
*                                                                               
         SPROG 10                                                               
         SSPEC H10,6,C'TRAFFIC ID AND TITLE'                                    
         SSPEC H10,45,C'LEN TALENT CID AND TITLE'                               
         SSPEC H10,86,C'METHOD   USE DATE PROGRAM NAME    NWK'                  
*                                                                               
         SPROG 20                                                               
         SSPEC H10,13,C'TRAFFIC ID AND TITLE'                                   
         SSPEC H10,52,C'LEN USE DATE PROGRAM NAME    NWK'                       
         SSPEC H10,85,C'REASON FOR CHANGE'                                      
*                                                                               
         SPROG 25                                                               
         SSPEC H10,6,C'TRAFFIC ID AND TITLE'                                    
         SSPEC H10,45,C'LEN TALENT CID AND TITLE'                               
         SSPEC H10,86,C'METHOD   USE DATE CNET   #USES'                         
*                                                                               
         SPROG 26                                                               
         SSPEC H10,13,C'TRAFFIC ID AND TITLE'                                   
         SSPEC H10,52,C'LEN USE DATE CNET   #USES'                              
         SSPEC H10,81,C'REASON FOR CHANGE'                                      
*                                                                               
         SPROG 27                                                               
         SSPEC H10,6,C'TRAFFIC ID AND TITLE'                                    
         SSPEC H10,45,C'LEN TALENT CID AND TITLE'                               
         SSPEC H10,86,C'METHOD   USE DATE MKT'                                  
*                                                                               
         SPROG 28                                                               
         SSPEC H10,36,C'TRAFFIC ID AND TITLE'                                   
         SSPEC H10,75,C'LEN USE DATE MKT'                                       
*                                                                               
         SPROG 29                                                               
         SSPEC H10,6,C'TRAFFIC ID AND TITLE'                                    
         SSPEC H10,45,C'LEN TALENT CID AND TITLE'                               
         SSPEC H10,86,C'METHOD   USE DATE CSYS'                                 
*                                                                               
         SPROG 30                                                               
         SSPEC H10,13,C'TRAFFIC ID AND TITLE'                                   
         SSPEC H10,52,C'LEN USE DATE CSYS'                                      
         SSPEC H10,81,C'REASON FOR CHANGE'                                      
*                                                                               
         DC    X'00'                                                            
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - SMTPERR'                              
***********************************************************************         
*                                                                     *         
*              ROUTINE TO EMAIL ERROR MESSAGES                        *         
*                                                                     *         
*NTRY    P1    A(ERROR MESSAGE)                                       *         
*        P2    A(RECORD)                                              *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
                                                                                
SMTPERR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,0(R1)            SAVE ERROR MESSAGE ADDRESS                   
         L     R3,4(R1)            SAVE RECORD ADDRESS                          
*                                                                               
*        INITIALIZE SMTP CONNECTION                                             
*                                                                               
         CLI   WSMTPSW,X'FF'       SKIP IF NOT FIRST EMAIL                      
         BE    SMERINIX                                                         
*                                                                               
         GOTOR =V(SMTP),DMCB,('SMTPAINI',0)  INITIALIZE SMTP                    
*                                                                               
*        BUILD SUBJECT LINE WITH FILE NAME                                      
*                                                                               
         MVC   SMERSUBJ+23(10),WHDMDCO   MEDIA COMPANY                          
*                                                                               
         LA    R1,SMERSUBJ+32                                                   
         LA    R0,10                                                            
*                                                                               
         CLI   0(R1),C' '          FIND LAST NON-BLANK                          
         BH    *+14                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         B     *+12                NO USERID                                    
         AHI   R1,1                NEXT AVAILABLE POSTION                       
         MVI   0(R1),C','                                                       
*                                                                               
         AHI   R1,1                NEXT AVAILABLE POSTION                       
*                                                                               
         MVC   0(2,R1),WHDDATE+4  MONTH                                         
         MVI   2(R1),C'/'                                                       
         MVC   3(2,R1),WHDDATE+6  DAY                                           
         MVI   5(R1),C'/'                                                       
         MVC   6(4,R1),WHDDATE    YEAR                                          
*                                                                               
         MVI   10(R1),C','                                                      
         AHI   R1,11                                                            
*                                                                               
         MVC   0(L'WHDFLNM,R1),WHDFLNM FILE NAME                                
*                                                                               
         LA    RF,SMERRECP         DEFAULT TO STANDARD RECIPIENTS               
*                                                                               
         CLI   EMAILOPT,C'Y'       IF E-MAIL OVERRIDE EXISTS                    
         BNE   *+8                                                              
         LA    RF,SVEMAIL             USE IT                                    
*                                                                               
         GOTOR =V(SMTP),DMCB,('SMTPAPRS',0(RF)),(80,SMERSUBJ)                   
*                                                                               
         MVI   WSMTPSW,X'FF'       SET INITIALIZED SWITCH                       
*                                                                               
SMERINIX DS    0H                                                               
*                                                                               
         MVC   SMERMSG1(50),0(R2)  ERROR MESSAGE                                
*                                                                               
         GOTOR =V(SMTP),DMCB,('SMTPAPTL',SMERMSG1)   PRINT MESSAGE              
*                                                                               
         MVC   SMERMSG2,SPACES                                                  
         GOTOR =V(SMTP),DMCB,('SMTPAPTL',SMERMSG2)   PRINT BLANK LINE           
*                                                                               
*        FIND LAYOUT FOR RECORD                                                 
*                                                                               
         LA    R4,LAYOUTTB         POINT TO TABLE OF RECORD LAYOUTS             
*                                                                               
SMERTBLP DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE IF NO MATCH                             
         BE    SMERTBDN                                                         
*                                                                               
         CLC   0(3,R4),0(R3)       FIND ENTRY FOR RECORD                        
         BE    SMERTBFD                                                         
*                                                                               
SMERTBCN DS    0H                                                               
         LA    R4,8(R4)            BUMP TO NEXT ENTRY IN TABLE                  
         B     SMERTBLP                                                         
*                                                                               
SMERTBDN DS    0H                                                               
         B     SMTPERRX                                                         
*                                                                               
SMERTBFD DS    0H                                                               
*                                                                               
         L     R4,4(R4)            POINT TO RECORD LAYOUT                       
*                                                                               
SMERRCLP DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE IF END OF TABLE                         
         BE    SMERRCDN                                                         
*                                                                               
         MVC   SMERMSG2,SPACES     INIT PRINT LINE                              
         MVC   SMERMSG2(30),0(R4)  PRINT FIELD TITLE                            
*                                                                               
         LLC   R1,30(R4)           GET DISPLACEMENT OF DATA INTO RECORD         
         LA    R1,0(R1,R3)         POINT TO DATA                                
         LLC   RF,31(R4)           GET LENGTH OF DATA                           
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SMERMSG2+30(0),0(R1) PRINT DATA                                  
*                                                                               
         GOTOR =V(SMTP),DMCB,('SMTPAPTL',SMERMSG2)   PRINT LINE                 
*                                                                               
SMERRCCN DS    0H                                                               
         LA    R4,32(R4)           BUMP TO NEXT TABLE ENTRY                     
         B     SMERRCLP                                                         
*                                                                               
SMERRCDN DS    0H                                                               
*                                                                               
         MVC   SMERMSG2,SPACES                                                  
         GOTOR =V(SMTP),DMCB,('SMTPAPTL',SMERMSG2)   PRINT BLANK LINE           
*                                                                               
         AP    WERRCTR,=P'1'       BUMP ERROR COUNTER                           
*                                                                               
         MVI   WBYPASS,C'Y'        SET TO BYPASS REST OF BATCH                  
*                                                                               
SMTPERRX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
***RRECP DC    CL80'US-TALENT_CLARUS_TEAM:'   RECIPIENTS                        
**ERRECP DC    CL80'US-TALENT MF DEV TEAM,US-TALENT PRODUCT TEAM:'              
SMERRECP DC    C'US-TALENT_MF_DEV_TEAM@MEDIAOCEAN.COM,US-TALENT_PRODUCT+        
               _TEAM@MEDIAOCEAN.COM,CLARUSOX@MEDIAOCEAN.COM,PRODUCT@TAL+        
               ENTPARTNERS.COM:'                                                
SMERSUBJ DC    CL80'NON-D|S CLARUS ERROR - '                                    
SMERMSG1 DC    CL80'ERROR IN RECORD XXXX: ERRMSG'                               
SMERMSG2 DC    CL80' '                                                          
                                                                                
         TITLE 'T70365 - NON-DDS CLARUS - ERRMSGS'                              
***********************************************************************         
*                                                                     *         
*        ERROR MESSAGES                                               *         
*                                                                     *         
***********************************************************************         
                                                                                
ERRUNKNW DC    CL50'UNKNOWN RECORD TYPE'                                        
ERRUID   DC    CL50'INVALID MEDIA COMPANY CODE'                                 
ERRDTE   DC    CL50'INVALID DATE'                                               
ERRTIME  DC    CL50'INVALID TIME'                                               
ERRFILE  DC    CL50'UNIQUE FILE NAME REQUIRED'                                  
ERRDUPE  DC    CL50'FILE UPLOADED PREVIOUSLY'                                   
ERRSTRDT DC    CL50'INVALID START RUN DATE'                                     
ERRENDDT DC    CL50'INVALID END RUN DATE'                                       
ERRUSEDT DC    CL50'INVALID USE DATE'                                           
ERRCOMLN DC    CL50'INVALID COMMERCIAL LENGTH'                                  
ERRCOMTI DC    CL50'INVALID COMMERCIAL TITLE'                                   
ERRINVID DC    CL50'INVALID ID CODE'                                            
ERRAGY   DC    CL50'INVALID AGENCY CODE'                                        
ERRCLI   DC    CL50'INVALID CLIENT CODE'                                        
ERRCLINM DC    CL50'INVALID CLIENT NAME'                                        
ERRPRD   DC    CL50'INVALID PRODUCT CODE'                                       
ERRPRDNM DC    CL50'INVALID PRODUCT NAME'                                       
ERRPRGNM DC    CL50'INVALID PROGRAM NAME'                                       
ERRCOM   DC    CL50'INVALID COMMERCIAL'                                         
ERRTRER1 DC    CL50'RECORD COUNT FIELD IS NOT NUMERIC'                          
ERRTRER2 DC    CL50'RECORD COUNT NOT EQUAL TO NUMBER OF RECORDS'                
ERRTRLE  DC    CL50'TRAILER RECORD MISSING'                                     
ERRTMKT  DC    CL50'INVALID TALENT MARKET'                                      
ERRTMKTN DC    CL50'INVALID TALENT MARKET NAME'                                 
ERRCSYS  DC    CL50'INVALID CABLE SYSTEM'                                       
ERRCSYSN DC    CL50'INVALID CABLE SYSTEM NAME'                                  
ERRCSTA  DC    CL50'INVALID CABLE STATION'                                      
ERRCSTAN DC    CL50'INVALID CABLE STATION NAME'                                 
ERRNETCD DC    CL50'INVALID NETWORK CODE'                                       
ERRUNMRK DC    CL50'INVALID UNMARKED'                                           
ERRNTAIR DC    CL50'INVALID NOT AIRED'                                          
ERRLENCH DC    CL50'INVALID LENGTH CHANGED'                                     
ERRPRDCH DC    CL50'INVALID PRODUCT CHANGED'                                    
ERRDTECH DC    CL50'INVALID DATE CHANGED'                                       
ERRCOMCH DC    CL50'INVALID COMMERCIAL CHANGED'                                 
ERRFLLTR DC    CL50'INVALID FLAT RATE/LATE NIGHT'                               
ERRREASN DC    CL50'INVALID REASON NOT AIRED'                                   
ERRMULTR DC    CL50'INVALID MULTIPLE RUN'                                       
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - RECORDS'                              
***********************************************************************         
*                                                                     *         
*        RECORD LAYOOUTS FOR ERROR MESSAGE                            *         
*                                                                     *         
*        DS    CL30'FIELD DESCRIPTION'                                *         
*        DS    AL1                 FIELD DISPLACEMENT                 *         
*        DS    AL1                 FIELD LENGTH                       *         
*                                                                     *         
***********************************************************************         
*                                                                               
LAYOUTTB DS    0D                  TABLE OF RECORD LAYOUTS                      
         DC    CL3'000',AL1(0),A(LAY000) HEADER RECORD                          
         DC    CL3'001',AL1(0),A(LAY001) TV WILDSPOT RECORD                     
         DC    CL3'002',AL1(0),A(LAY002) RADIO WILDSPOT RECORD                  
         DC    CL3'003',AL1(0),A(LAY003) LOCAL CABLE RECORD                     
         DC    CL3'004',AL1(0),A(LAY004) NETWORK CABLE RECORD                   
         DC    CL3'005',AL1(0),A(LAY005) NETWORK RECORD                         
         DC    CL3'999',AL1(0),A(LAY999) TRAILER RECORD                         
         DC    X'FF'               EOT                                          
*                                                                               
LAYOUTS  DS    0D                                                               
LAY000   DS    0D                  LAYOUT OF HEADER RECORD                      
  DC    CL30'RECORD TYPE',AL1(NDHDID-NDHDD),AL1(L'NDHDID)                       
  DC    CL30'MEDIA COMPANY ID',AL1(NDHDMDCO-NDHDD),AL1(L'NDHDMDCO)              
  DC    CL30'DATE',AL1(NDHDDATE-NDHDD),AL1(L'NDHDDATE)                          
  DC    CL30'TIME',AL1(NDHDTIME-NDHDD),AL1(L'NDHDTIME)                          
  DC    CL30'FILENAME',AL1(NDHDFLNM-NDHDD),AL1(L'NDHDFLNM)                      
  DC    CL30'MEDIA COMPANY NAME',AL1(NDHDMDNM-NDHDD),AL1(L'NDHDMDNM)            
         DC    X'FF'               EOT                                          
*                                                                               
LAY001   DS    0D                  LAYOUT OF TV WILDSPOT RECORD                 
  DC CL30'RECORD TYPE',AL1(NDTWID-NDTWD),AL1(L'NDTWID)                          
  DC CL30'UNIQUE ID',AL1(NDTWUPID-NDTWD),AL1(L'NDTWUPID)                        
  DC CL30'TALENT AGENCY CODE',AL1(NDTWAGY-NDTWD),AL1(L'NDTWAGY)                 
  DC CL30'COMMERCIAL ID/AD-ID',AL1(NDTWCMID-NDTWD),AL1(L'NDTWCMID)              
  DC CL30'COMMERCIAL LENGTH IN SECONDS',AL1(NDTWCMLN-NDTWD)                     
  DC AL1(L'NDTWCMLN)                                                            
  DC CL30'COMMERCIAL TITLE',AL1(NDTWCMTL-NDTWD),AL1(L'NDTWCMTL)                 
  DC CL30'CLIENT CODE',AL1(NDTWCLCD-NDTWD),AL1(L'NDTWCLCD)                      
  DC CL30'CLIENT NAME',AL1(NDTWCLNM-NDTWD),AL1(L'NDTWCLNM)                      
  DC CL30'PRODUCT CODE',AL1(NDTWPRCD-NDTWD),AL1(L'NDTWPRCD)                     
  DC CL30'PRODUCT NAME',AL1(NDTWPRNM-NDTWD),AL1(L'NDTWPRNM)                     
  DC CL30'ACTIVE START DATE',AL1(NDTWASTR-NDTWD),AL1(L'NDTWASTR)                
  DC CL30'ACTIVE END DATE',AL1(NDTWAEND-NDTWD),AL1(L'NDTWAEND)                  
  DC CL30'TALENT MARKET CODE',AL1(NDTWMKCD-NDTWD),AL1(L'NDTWMKCD)               
  DC CL30'MARKET NAME',AL1(NDTWMKNM-NDTWD),AL1(L'NDTWMKNM)                      
         DC    X'FF'               EOT                                          
*                                                                               
LAY002   DS    0D                  LAYOUT OF RADIO WILDSPOT RECORD              
  DC CL30'RECORD TYPE',AL1(NDRWID-NDRWD),AL1(L'NDRWID)                          
  DC CL30'UNIQUE ID',AL1(NDRWUPID-NDRWD),AL1(L'NDRWUPID)                        
  DC CL30'TALENT AGENCY CODE',AL1(NDRWAGY-NDRWD),AL1(L'NDRWAGY)                 
  DC CL30'COMMERCIAL ID/AD-ID',AL1(NDRWCMID-NDRWD),AL1(L'NDRWCMID)              
  DC CL30'COMMERCIAL LENGTH IN SECONDS',AL1(NDRWCMLN-NDRWD)                     
  DC AL1(L'NDRWCMLN)                                                            
  DC CL30'COMMERCIAL TITLE',AL1(NDRWCMTL-NDRWD),AL1(L'NDRWCMTL)                 
  DC CL30'CLIENT CODE',AL1(NDRWCLCD-NDRWD),AL1(L'NDRWCLCD)                      
  DC CL30'CLIENT NAME',AL1(NDRWCLNM-NDRWD),AL1(L'NDRWCLNM)                      
  DC CL30'PRODUCT CODE',AL1(NDRWPRCD-NDRWD),AL1(L'NDRWPRCD)                     
  DC CL30'PRODUCT NAME',AL1(NDRWPRNM-NDRWD),AL1(L'NDRWPRNM)                     
  DC CL30'ACTIVE START DATE',AL1(NDRWASTR-NDRWD),AL1(L'NDRWASTR)                
  DC CL30'ACTIVE END DATE',AL1(NDRWAEND-NDRWD),AL1(L'NDRWAEND)                  
  DC CL30'TALENT MARKET CODE',AL1(NDRWMKCD-NDRWD),AL1(L'NDRWMKCD)               
  DC CL30'MARKET NAME',AL1(NDRWMKNM-NDRWD),AL1(L'NDRWMKNM)                      
         DC    X'FF'               EOT                                          
*                                                                               
LAY003   DS    0D                  LAYOUT OF LOCAL CABLE RECORD                 
  DC CL30'RECORD TYPE',AL1(NDLCID-NDLCD),AL1(L'NDLCID)                          
  DC CL30'UNIQUE ID',AL1(NDLCUPID-NDLCD),AL1(L'NDLCUPID)                        
  DC CL30'TALENT AGENCY CODE',AL1(NDLCAGY-NDLCD),AL1(L'NDLCAGY)                 
  DC CL30'COMMERCIAL ID/AD-ID',AL1(NDLCCMID-NDLCD),AL1(L'NDLCCMID)              
  DC CL30'COMMERCIAL LENGTH IN SECONDS',AL1(NDLCCMLN-NDLCD)                     
  DC AL1(L'NDLCCMLN)                                                            
  DC CL30'COMMERCIAL TITLE',AL1(NDLCCMTL-NDLCD),AL1(L'NDLCCMTL)                 
  DC CL30'CLIENT CODE',AL1(NDLCCLCD-NDLCD),AL1(L'NDLCCLCD)                      
  DC CL30'CLIENT NAME',AL1(NDLCCLNM-NDLCD),AL1(L'NDLCCLNM)                      
  DC CL30'PRODUCT CODE',AL1(NDLCPRCD-NDLCD),AL1(L'NDLCPRCD)                     
  DC CL30'PRODUCT NAME',AL1(NDLCPRNM-NDLCD),AL1(L'NDLCPRNM)                     
  DC CL30'ACTIVE START DATE',AL1(NDLCASTR-NDLCD),AL1(L'NDLCASTR)                
  DC CL30'ACTIVE END DATE',AL1(NDLCAEND-NDLCD),AL1(L'NDLCAEND)                  
  DC CL30'CABLE SYSTEM CODE',AL1(NDLCSYCD-NDLCD),AL1(L'NDLCSYCD)                
  DC CL30'CABLE SYSTEM NAME',AL1(NDLCSYNM-NDLCD),AL1(L'NDLCSYNM)                
         DC    X'FF'               EOT                                          
*                                                                               
LAY004   DS    0D                  LAYOUT OF NETWORK CABLE RECORD               
  DC CL30'RECORD TYPE',AL1(NDCBID-NDCBD),AL1(L'NDCBID)                          
  DC CL30'UNIQUE ID',AL1(NDCBUPID-NDCBD),AL1(L'NDCBUPID)                        
  DC CL30'TALENT AGENCY CODE',AL1(NDCBAGY-NDCBD),AL1(L'NDCBAGY)                 
  DC CL30'COMMERCIAL ID/AD-ID',AL1(NDCBCMID-NDCBD),AL1(L'NDCBCMID)              
  DC CL30'COMMERCIAL LENGTH IN SECONDS',AL1(NDCBCMLN-NDCBD)                     
  DC AL1(L'NDCBCMLN)                                                            
  DC CL30'COMMERCIAL TITLE',AL1(NDCBCMTL-NDCBD),AL1(L'NDCBCMTL)                 
  DC CL30'CLIENT CODE',AL1(NDCBCLCD-NDCBD),AL1(L'NDCBCLCD)                      
  DC CL30'CLIENT NAME',AL1(NDCBCLNM-NDCBD),AL1(L'NDCBCLNM)                      
  DC CL30'PRODUCT CODE',AL1(NDCBPRCD-NDCBD),AL1(L'NDCBPRCD)                     
  DC CL30'PRODUCT NAME',AL1(NDCBPRNM-NDCBD),AL1(L'NDCBPRNM)                     
  DC CL30'USE DATE',AL1(NDCBUDTE-NDCBD),AL1(L'NDCBUDTE)                         
  DC CL30'CABLE STATION CODE',AL1(NDCBSTCD-NDCBD),AL1(L'NDCBSTCD)               
  DC CL30'CABLE STATION NAME',AL1(NDCBSTNM-NDCBD),AL1(L'NDCBSTNM)               
         DC    X'FF'               EOT                                          
*                                                                               
LAY005   DS    0D                  LAYOUT OF NETWORK RECORD                     
  DC CL30'RECORD TYPE',AL1(NDNTID-NDNTD),AL1(L'NDNTID)                          
  DC CL30'UNIQUE ID',AL1(NDNTUPID-NDNTD),AL1(L'NDNTUPID)                        
  DC CL30'TALENT AGENCY CODE',AL1(NDNTAGY-NDNTD),AL1(L'NDNTAGY)                 
  DC CL30'COMMERCIAL ID/AD-ID',AL1(NDNTCMID-NDNTD),AL1(L'NDNTCMID)              
  DC CL30'COMMERCIAL LENGTH IN SECONDS',AL1(NDNTCMLN-NDNTD)                     
  DC AL1(L'NDNTCMLN)                                                            
  DC CL30'COMMERCIAL TITLE',AL1(NDNTCMTL-NDNTD),AL1(L'NDNTCMTL)                 
  DC CL30'CLIENT CODE',AL1(NDNTCLCD-NDNTD),AL1(L'NDNTCLCD)                      
  DC CL30'CLIENT NAME',AL1(NDNTCLNM-NDNTD),AL1(L'NDNTCLNM)                      
  DC CL30'PRODUCT CODE',AL1(NDNTPRCD-NDNTD),AL1(L'NDNTPRCD)                     
  DC CL30'PRODUCT NAME',AL1(NDNTPRNM-NDNTD),AL1(L'NDNTPRNM)                     
  DC CL30'USE DATE',AL1(NDNTUDTE-NDNTD),AL1(L'NDNTUDTE)                         
  DC CL30'PROGRAM NAME',AL1(NDNTPGNM-NDNTD),AL1(L'NDNTPGNM)                     
  DC CL30'NETWORK CODE',AL1(NDNTNTCD-NDNTD),AL1(L'NDNTNTCD)                     
  DC CL30'UNMARKED',AL1(NDNTUNMK-NDNTD),AL1(L'NDNTUNMK)                         
  DC CL30'NOT AIRED',AL1(NDNTNAIR-NDNTD),AL1(L'NDNTNAIR)                        
  DC CL30'LENGTH CHANGED',AL1(NDNTLCHG-NDNTD),AL1(L'NDNTLCHG)                   
  DC CL30'PRODUCT CHANGED',AL1(NDNTPCHG-NDNTD),AL1(L'NDNTPCHG)                  
  DC CL30'DATE CHANGED',AL1(NDNTDCHG-NDNTD),AL1(L'NDNTDCHG)                     
  DC CL30'COMMERCIAL CHANGED',AL1(NDNTCCHG-NDNTD),AL1(L'NDNTCCHG)               
  DC CL30'FLAT RATE/LATE NIGHT',AL1(NDNTFLAT-NDNTD),AL1(L'NDNTFLAT)             
  DC CL30'REASON NOT AIRED',AL1(NDNTNWHY-NDNTD),AL1(L'NDNTNWHY)                 
  DC CL30'MULTIPLE RUN',AL1(NDNTMRUN-NDNTD),AL1(L'NDNTMRUN)                     
         DC    X'FF'               EOT                                          
*                                                                               
LAY999   DS    0D                  LAYOUT OF TRAILER RECORD                     
         DC    CL30'RECORD TYPE',AL1(NDTRID-NDTRD),AL1(L'NDTRID)                
         DC    CL30'RECORD COUNT',AL1(NDTRCT-NDTRD),AL1(L'NDTRCT)               
         DC    X'FF'               EOT                                          
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - OPENWRKR'                             
***********************************************************************         
*                                                                     *         
*  WORKER INTERFACE                                                   *         
*                                                                     *         
***********************************************************************         
*                                                                               
OPENWRKR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ID,ID               INIT WORKER FILES                            
*                                                                               
         LA    RE,IOWRKR                                                        
         LA    RF,L'IOWRKR                                                      
         XCEF                                                                   
*                                                                               
         LA    R3,ID                                                            
         USING UKRECD,R3           ESTABLISH WORKER FILE ID                     
*                                                                               
         MVC   UKUSRID,WHDUID      ORIGINATOR                                   
*                                                                               
         MVC   UKSYSPRG,=C'STA'    SYSTEM PROGRAM                               
         MVI   UKSUBPRG,C'N'       SUB PROGRAM                                  
*                                                                               
         CLI   SRTTYPE,C'N'        IF A NETWORK RECORD                          
         BNE   *+14                                                             
         MVC   UKSYSPRG,=C'NE1'       RESET SYSTEM PROGRAM                      
         MVI   UKSUBPRG,C'N'          AND SUB PROGRAM                           
*                                                                               
         MVC   UKDAY,WTODAY+2      DAY                                          
         MVI   UKCLASS,C'T'        CLASS                                        
         OI    UKFLAG,X'01'                    ALLOW DUPLICATE KEYS             
         OI    UKFLAG,X'10'                    RETENTION DAYS                   
*                                                                               
         MVC   COMMAND,=CL6'OPEN'  COMMAND IS OPEN                              
*                                                                               
         LA    R1,IOWRKR+28                                                     
         USING WKRECD,R1                                                        
*                                                                               
         MVC   WKRETN,=H'7'        RETAIN 7 DAYS                                
*                                                                               
         DROP  R1                                                               
*                                                                               
         LA     R3,IOWRKR                                                       
         LAY    R4,WRKRBUFF                                                     
*                                                                               
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
*                                                                               
         TM    DMCB+8,X'C0'        NO ERRORS ALLOWED                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   WWRKRSW,X'FF'       TURN ODD WORKER FILE OPEN SWITCH             
*                                                                               
OPENWRKX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3                                                               
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - MEDCPYNM'                             
***********************************************************************         
*                                                                     *         
*        MOVE ELEMENTS TO WORKER OUTAREA                              *         
*                                                                     *         
***********************************************************************         
*                                                                               
MEDCPYNM NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TAFND,R6            ESTABLISH AS COMMENT ELEMENT                 
         MVI   TAFNEL,TAFNELQ      SET ELEMENT ID                               
         MVI   TAFNTYPE,TAFNTMCN   SET AS MEDIA COMPANY NAME                    
*                                                                               
         CLI   MDNMLEN,0                                                        
         BNE   MCN100                                                           
         LHI   RE,36                                                            
         LA    RF,WHDMDNM+L'WHDMDNM-1 END OF MEDIA COMPANY NAME                 
*                                                                               
MCN050   CLI   0(RF),C' '          FIND LAST CHARACTER OF NAME                  
         BH    MCN090                                                           
         MVI   0(RF),0                                                          
         AHI   RF,-1                                                            
         BCT   RE,MCN050                                                        
*                                                                               
MCN090   STC   RE,MDNMLEN          SAVE LENGTH                                  
*                                                                               
MCN100   DS    0H                                                               
         LLC   RF,MDNMLEN                                                       
         AHI   RF,-1                                                            
         EX    RF,MCNMOVE          MOVE THE NAME OVER                           
*                                                                               
         AHI   RF,TAFNLNQ+1                                                     
         STC   RF,TAFNLEN          SAVE ELEMENT LENGTH                          
*                                                                               
         XIT1                                                                   
*                                                                               
MCNMOVE  MVC   TAFNNAME(0),WHDMDNM SET MEDIA COMPANY NAME                       
         LTORG                                                                  
         DROP  R6                                                               
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - MOVEWRKR'                             
***********************************************************************         
*                                                                     *         
*        MOVE ELEMENTS TO WORKER OUTAREA                              *         
*                                                                     *         
***********************************************************************         
*                                                                               
MOVEWRKR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,IOWRKR           INIT WORKER FILE IO AREA                     
         LA    RF,L'IOWRKR                                                      
         XCEF                                                                   
*                                                                               
         LA    RE,IOWDATA          MOVE ELEMENTS TO WORKER OUT AREA             
         LHI   RF,L'IOWDATA                                                     
         LA    R0,MYWORK                                                        
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R3,IOWDATA                                                       
         SR    R1,R1                                                            
*                                                                               
MWRK10   LLC   R2,1(R3)            GET ELEMENT LENGTH                           
         AR    R1,R2               ADD TO TOTAL ELEM LENGTH                     
         AR    R3,R2               BUMP TO NEXT ELEMENT                         
*                                                                               
         CLI   1(R3),0             ANY MORE ELEMENTS                            
         BNE   MWRK10                                                           
*                                                                               
         LA    R1,4(R1)            NO/ADD 4 BYTE IOWLEN HEADER                  
         STH   R1,IOWLEN       SET TOTAL ELEM LENGTH IN WORKER HEADER           
*                                                                               
MOVEWRKX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - ADDWRKR'                              
***********************************************************************         
*                                                                     *         
*        ADD  ELEMENTS TO WORKER BUFFER                               *         
*                                                                     *         
***********************************************************************         
*                                                                               
ADDWRKR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   TRACOPT,C'Y'        SKIP IF NOT TRACING                          
         BNE   ADDWRKR1                                                         
*                                                                               
         MVC   P(6),=C'ADDELM'     SET ACTION CODE                              
         BRAS  RE,SPLAT                                                         
*                                                                               
         SR    RF,RF               GET RECORD LENGTH                            
         ICM   RF,3,IOWLEN                                                      
*                                                                               
         GOTO1 HEXOUT,DMCB,IOWDATA,P,(RF) PRINT RECORD                          
*                                                                               
         BRAS  RE,SPLAT                                                         
*                                                                               
ADDWRKR1 DS    0H                                                               
*                                                                               
         MVC   COMMAND,=CL6'ADD'                                                
         LA    R3,IOWRKR                                                        
         LAY   R4,WRKRBUFF                                                      
*                                                                               
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
*                                                                               
         TM    DMCB+8,X'C0'        CHECK FOR ERRORS                             
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ADDWRKRX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - CLSWRKR'                              
***********************************************************************         
*                                                                     *         
*        CLOSE WORKER FILE                                            *         
*                                                                     *         
***********************************************************************         
*                                                                               
CLOSWRKR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   COMMAND,=CL6'CLOSE'                                              
         LA     R3,IOWRKR                                                       
         LAY    R4,WRKRBUFF                                                     
*                                                                               
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
*                                                                               
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   COMMAND,=CL8'RETAIN'                                             
*                                                                               
         LA    R1,IOWRKR+28                                                     
         USING WKRECD,R1                                                        
         MVC   WKRETN,=H'7'                                                     
         DROP  R1                                                               
*                                                                               
         LA    R3,IOWRKR                                                        
         LAY   R4,WRKRBUFF                                                      
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
*                                                                               
         TM    DMCB+8,X'C0'        NO ERRORS ALLOWED                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   WWRKRSW,0           TURN OFF WORKER FILE OPEN SWITCH             
*                                                                               
CLOSWRKX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        VALIDATE MARKET, NETWORK STATION, SYSTEM CODES               *         
*           BYTE = MEDIA CODE                                         *         
*           DUB  = CODE                                               *         
*                                                                     *         
***********************************************************************         
*                                                                               
VALMCS   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLMTD,R4                                                         
         MVI   TLMTCD,TLMTCDQ      CNET/CSYS/MARKET RECORD                      
         MVC   TLMTTYPE,BYTE       MEDIA                                        
         MVC   TLMTCODE,DUB                                                     
         LA    R1,6                                                             
         LA    RF,TLMTCODE                                                      
VALMCS5  CLI   0(RF),C'A'                                                       
         BNL   *+8                                                              
         MVI   0(RF),0             GET RID OF SPACES                            
         AHI   RF,1                                                             
         BCT   R1,VALMCS5                                                       
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(TLMTINUM-TLMTD),KEYSAVE                                      
         BE    VALMCSY                                                          
*                                                                               
         LTR   RB,RB               CC = NO                                      
         B     *+6                                                              
VALMCSY  CR    RB,RB               CC = YES                                     
VALMCSX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CLEAR MYWORK                                                 *         
***********************************************************************         
INITWRK  NTR1  BASE=*,LABEL=*                                                   
         LA    RE,MYWORK           INIT WORKER FILE AREA                        
         LHI   RF,L'MYWORK                                                      
         XCEF                                                                   
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
         TITLE 'T70365 - NON-DDS CLARUS - SSB'                                  
***********************************************************************         
*                                                                     *         
*        FAKE SSB                                                     *         
*                                                                     *         
***********************************************************************         
                                                                                
         DS    0D                                                               
         DC    CL8'**SSB **'                                                    
SSB      DS    0X                                                               
         DC    X'0000FF',X'00',XL252'00'                                        
                                                                                
         TITLE 'T70365 - NON-DDS CLARUS - WRKRBUFF'                             
***********************************************************************         
*                                                                     *         
*        WORKER FILE BUFFER                                           *         
*                                                                     *         
***********************************************************************         
*                                                                               
WRKRBUFF DS    0D                  ALIGNMENT                                    
         DC    4500X'00'           WORKER BUFFER                                
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - NDHDRD'                               
***********************************************************************         
*                                                                     *         
*        NON-DDS CLARUS UPLOAD HEADER RECORD                          *         
*                                                                     *         
***********************************************************************         
                                                                                
NDHDD    DSECT                     FILE HEADER RECORD                           
NDHDR    DS    0C                  FILE HEADER RECORD                           
NDHDID   DS    CL3                 RECORD ID                                    
NDHDIDQ  EQU   C'000'                FILE HEADER                                
NDHDMDCO DS    CL10                MEDIA COMPANY ID                             
NDHDDATE DS    CL8                 DATE - YYYYMMDD                              
NDHDTIME DS    CL8                 TIME - HH:MM:SS                              
NDHDFLNM DS    CL36                UNIQUE FILE NAME                             
NDHDMDNM DS    CL36                MEDIA COMPANY NAME                           
NDHDRLN  EQU   *-NDHDR             LENGTH OF RECORD                             
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - NDTWD'                                
***********************************************************************         
*                                                                     *         
*        NON-DDS CLARUS UPLOAD TV -WILDSPOT                           *         
*                                                                     *         
***********************************************************************         
                                                                                
NDTWD    DSECT                     TV WILD SPOT DSECT                           
NDTWREC  DS    0C                  TV WILD SPOT RECORD                          
NDTWID   DS    CL3                 RECORD ID                                    
NDTWIDQ  EQU   C'001'                TV WILDSPOT                                
NDTWUPID DS    CL10                UNIQUE ID                                    
NDTWAGY  DS    CL6                 TALENT AGENCY CODE                           
NDTWCMID DS    CL12                COMMERCIAL ID/AD-ID                          
NDTWCMLN DS    CL3                 COMMERCIAL LENGTH IN SECONDS                 
NDTWCMTL DS    CL36                COMMERCIAL TITLE                             
NDTWCLCD DS    CL6                 CLIENT  CODE                                 
NDTWCLNM DS    CL36                CLIENT  NAME                                 
NDTWPRCD DS    CL6                 PRODUCT CODE                                 
NDTWPRNM DS    CL36                PRODUCT NAME                                 
NDTWASTR DS    CL8                 ACTIVE START DATE - YYYYMMDD                 
NDTWAEND DS    CL8                 ACTIVE END   DATE - YYYYMMDD                 
NDTWMKCD DS    CL4                 MARKET CODE - TALENT                         
NDTWMKNM DS    CL15                MARKET NAME                                  
NDTWLN   EQU   *-NDTWREC           LENGTH OF RECORD                             
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - NDRWD'                                
***********************************************************************         
*                                                                     *         
*        NON-DDS CLARUS UPLOAD - RADIO WILDSPOT                       *         
*                                                                     *         
***********************************************************************         
                                                                                
NDRWD    DSECT                     RADIO WILDSPOT DSECT                         
NDRWREC  DS    0C                  RADIO WILD SPOT RECORD                       
NDRWID   DS    CL3                 RECORD ID                                    
NDRWIDQ  EQU   C'002'                RADIO WILDSPOT                             
NDRWUPID DS    CL10                UNIQUE ID                                    
NDRWAGY  DS    CL6                 TALENT AGENCY CODE                           
NDRWCMID DS    CL12                COMMERCIAL ID/AD-ID                          
NDRWCMLN DS    CL3                 COMMERCIAL LENGTH IN SECONDS                 
NDRWCMTL DS    CL36                COMMERCIAL TITLE                             
NDRWCLCD DS    CL6                 CLIENT  CODE                                 
NDRWCLNM DS    CL36                CLIENT  NAME                                 
NDRWPRCD DS    CL6                 PRODUCT CODE                                 
NDRWPRNM DS    CL36                PRODUCT NAME                                 
NDRWASTR DS    CL8                 ACTIVE START DATE - YYYYMMDD                 
NDRWAEND DS    CL8                 ACTIVE END   DATE - YYYYMMDD                 
NDRWMKCD DS    CL4                 MARKET CODE - ALPHA                          
NDRWMKNM DS    CL15                MARKET NAME                                  
NDRWLN   EQU   *-NDRWREC           LENGTH OF RECORD                             
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - NDLCD'                                
***********************************************************************         
*                                                                     *         
*        NON-DDS CLARUS UPLOAD - LOCAL CABLE                          *         
*                                                                     *         
***********************************************************************         
                                                                                
NDLCD    DSECT                     LOCAL CABLE DSECT                            
NDLCREC  DS    0C                  LOCAL CABLE RECORD                           
NDLCID   DS    CL3                 RECORD ID                                    
NDLCIDQ  EQU   C'003'               LOCAL CABLE                                 
NDLCUPID DS    CL10                UNIQUE ID                                    
NDLCAGY  DS    CL6                 TALENT AGENCY CODE                           
NDLCCMID DS    CL12                COMMERCIAL ID/AD-ID                          
NDLCCMLN DS    CL3                 COMMERCIAL LENGTH IN SECONDS                 
NDLCCMTL DS    CL36                COMMERCIAL TITLE                             
NDLCCLCD DS    CL6                 CLIENT  CODE                                 
NDLCCLNM DS    CL36                CLIENT  NAME                                 
NDLCPRCD DS    CL6                 PRODUCT CODE                                 
NDLCPRNM DS    CL36                PRODUCT NAME                                 
NDLCASTR DS    CL8                 ACTIVE START DATE - YYYYMMDD                 
NDLCAEND DS    CL8                 ACTIVE END   DATE - YYYYMMDD                 
NDLCSYCD DS    CL6                 LOCAL CABLE SYSTEM - CODE                    
NDLCSYNM DS    CL15                LOCAL CABLE SYSTEM - NAME                    
NDLCLN   EQU   *-NDLCREC           LENGTH OF RECORD                             
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - NDCBD'                                
***********************************************************************         
*                                                                     *         
*        NON-DDS CLARUS UPLOAD - CABLE                                *         
*                                                                     *         
***********************************************************************         
                                                                                
NDCBD    DSECT                     CABLE DSECT                                  
NDCBREC  DS    0C                  CABLE RECORD                                 
NDCBID   DS    CL3                 RECORD ID                                    
NDCBIDQ  EQU   C'004'               CABLE                                       
NDCBUPID DS    CL10                UNIQUE ID                                    
NDCBAGY  DS    CL6                 TALENT AGENCY CODE                           
NDCBCMID DS    CL12                COMMERCIAL ID/AD-ID                          
NDCBCMLN DS    CL3                 COMMERCIAL LENGTH IN SECONDS                 
NDCBCMTL DS    CL36                COMMERCIAL TITLE                             
NDCBCLCD DS    CL6                 CLIENT  CODE                                 
NDCBCLNM DS    CL36                CLIENT  NAME                                 
NDCBPRCD DS    CL6                 PRODUCT CODE                                 
NDCBPRNM DS    CL36                PRODUCT NAME                                 
NDCBUDTE DS    CL8                 USE DATE - YYYYMMDD                          
NDCBSTCD DS    CL4                 CABLE STATION CODE - NTI                     
NDCBSTNM DS    CL15                CABLE STATION NAME                           
NDCBLN   EQU   *-NDCBREC           LENGTH OF RECORD                             
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - NDNTD'                                
***********************************************************************         
*                                                                     *         
*        NON-DDS CLARUS UPLOAD - NETWORK                              *         
*                                                                     *         
***********************************************************************         
                                                                                
NDNTD    DSECT                     NETWORK DSECT                                
NDNTREC  DS    0C                  NETWORK RECORD                               
NDNTID   DS    CL3                 RECORD ID                                    
NDNTIDQ  EQU   C'005'               CABLE                                       
NDNTUPID DS    CL10                UNIQUE ID                                    
NDNTAGY  DS    CL6                 TALENT AGENCY CODE                           
NDNTCMID DS    CL12                COMMERCIAL ID/AD-ID                          
NDNTCMLN DS    CL3                 COMMERCIAL LENGTH IN SECONDS                 
NDNTCMTL DS    CL36                COMMERCIAL TITLE                             
NDNTCLCD DS    CL6                 CLIENT  CODE                                 
NDNTCLNM DS    CL36                CLIENT  NAME                                 
NDNTPRCD DS    CL6                 PRODUCT CODE                                 
NDNTPRNM DS    CL36                PRODUCT NAME                                 
NDNTUDTE DS    CL8                 USE DATE - YYYYMMDD                          
NDNTPGNM DS    CL15                PROGRAM NAME                                 
NDNTNTCD DS    CL1                 NETWORK CODE                                 
*                                    A - ABC                                    
*                                    C - CBS                                    
*                                    F - FOX                                    
*                                    I - ITN                                    
*                                    M - MNT OR MY                              
*                                    N - NBC                                    
*                                    X - PAX                                    
*                                    U - UPN                                    
*                                    W - WB OR CW                               
*                                    S - SYNDICATED                             
NDNTUNMK DS    CL1                 UNMARKED - T/F                               
NDNTNAIR DS    CL1                 UNAIRED  - T/F                               
NDNTLCHG DS    CL1                 LENGTH     CHANGED  - T/F                    
NDNTPCHG DS    CL1                 PRODUCT    CHANGED  - T/F                    
NDNTDCHG DS    CL1                 DATE       CHANGED  - T/F                    
NDNTCCHG DS    CL1                 COMMERCIAL CHANGED  - T/F                    
NDNTFLAT DS    CL1                 FLAT RATE/LATE NIGHT- T/F                    
NDNTNWHY DS    CL1                 REASON NOT AIRED                             
*                                    D - DELETED                                
*                                    P - PREEMPTED                              
*                                    M - MISSED                                 
NDNTMRUN DS    CL1                 MULTIPLE RUN        - T/F                    
NDNTLN   EQU   *-NDNTREC           LENGTH OF RECORD                             
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - NDTRD'                                
***********************************************************************         
*                                                                     *         
*        NON-DDS CLARUS UPLOAD - TRAILER                              *         
*                                                                     *         
***********************************************************************         
                                                                                
NDTRD    DSECT                     TRAILER DSECT                                
NDTRREC  DS    0C                  TRAILER RECORD                               
NDTRID   DS    CL3                 RECORD ID                                    
NDTRIDQ  EQU   C'999'               CABLE                                       
NDTRCT   DS    CL10                RECORD COUNT                                 
NDTRLN   EQU   *-NDTRREC           LENGTH OF RECORD                             
*                                                                               
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - MYD'                                  
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
                                                                                
MYD      DSECT                                                                  
*                                                                               
PARMS    DS    6F                  PARAMETER LIST                               
*                                                                               
ADTRPACK DS    A                   PACKS COMMERCIAL IDS                         
*                                                                               
WKEY     DS    XL32                KEY SAVEAREA                                 
WFLKYSV  DS    CL48                SYSTEM KEY SAVEAREA                          
WRECCTR  DS    PL8                 RECORD COUNTER                               
WERRCTR  DS    PL8                 ERROR  COUNTER                               
WREADSW  DS    CL1                 READ SWITCH - 'A' - ADD RECORD               
*                                                'P' - PUT RECORD               
WRERUNOP DS    CL1                 C'Y' - RERUNING PROCESSING                   
WBYPASS  DS    CL1                 C'Y' - BYPASS BATCH                          
*                                                                               
*        FIELDS SAVED FROM HEADER RECORD                                        
*                                                                               
WHDSAVE  DS    0X                  HEADER DATA SAVEAREA                         
WHDMDCO  DS    CL10                MEDIA COMPANY ID                             
WHDUID   DS    CL2                 USERID                                       
WHDDATE  DS    CL8                 DATE - YYYYMMDD                              
WHDDATEP DS    XL3                 DATE - YMD - PWOS                            
WHDTIME  DS    CL8                 TIME - HH:MM:SS                              
WHDTIMEP DS    XL3                 TIME - HMS - PWOS                            
WHDFLNM  DS    CL36                UNIQUE FILE NAME                             
WHDMDNM  DS    CL36                MEDIA COMPANY NAME                           
WHDSAVEL EQU   *-WHDSAVE           LENGTH OF HEADER SAVEAREA                    
*                                                                               
WAGY     DS    CL6                 WORK AGENCY CODE                             
WAGYNAME DS    CL36                WORK AGENCY NAME                             
WDATE    DS    CL3                 WORK DATE - YMD PWOS                         
WDATECH  DS    CL6                 WORK DATE - YYMMDD - CHARACTER               
WCID     DS    CL12                INTERNAML COMMERCIAL NUMBER                  
WNPSEQ   DS    XL1                 TANPELM SEQ NUMBER WORKAREA                  
WTODAY   DS    XL3                 TODAY - PWOS                                 
*                                                                               
WINITSW  DS    XL1                 X'FF' - INIT ALREADY DONE                    
WSORTSW  DS    XL1                 X'FF' - SORT INIT ALREADY DONE               
WWRKRSW  DS    XL1                 X'FF' - WORKER FILE OPEN                     
WSMTPSW  DS    CL1                 X'FF' - SMTP INITIALIZED                     
*                                                                               
TRACOPT  DS    CL1                 C'Y' - TRACE                                 
EMAILOPT DS    CL1                 C'Y' - EMAIL RECIPIENTS OVERRIDE             
REPTOPT  DS    CL1                 C'Y' - PRINT A REPORT                        
*                                                                               
NEWCOM   DS    XL(L'TANXCOM)       INTERNAL COMMERCIAL NUMBER                   
NEWVER   DS    CL1                 NEW COMMERCIAL VERSION LETTER                
NEWCTYPE DS    XL(L'TANXTYPE)      COMMERCIAL TYPE                              
*                                                                               
USECNT   DS    XL2                 USE COUNT                                    
MDNMLEN  DS    XL1                 MEDIA COMPANY NAME LENGTH                    
*                                                                               
WMTCH    DS    XL1                 MATCHED COMMERCIAL SWITCH                    
*                                  1 - MATCHED, 0 - UNMATCHED                   
WCOMTTL  DS    CL36                COMMERCIAL TITLE                             
*                                                                               
SRTKEYSV DS    XL(SRTKEYLN)        SORT KEY SAVEAREA                            
*                                                                               
*        FIELDS FOR WORKER FILES                                                
*                                                                               
         DS    0D                  ALIGNMENT                                    
ID       DS    CL16                                                             
IOWRKR   DS    CL336                                                            
         ORG   IOWRKR                                                           
IOWLEN   DS    CL4                 LENGTH OF WORKER RECORD                      
IOWDATA  DS    CL332               DATA                                         
IOWRKND  DS    CL2                 SET TO ZERO                                  
*                                                                               
         DS    0D                                                               
SVHDR    DS    CL200               FILE HEADER SAVEAREA                         
SVEMAIL  DS    XL80                ALTERNATE E-MAIL ADDRESS                     
*                                                                               
         DS    0D                  ALIGNMENT                                    
UPLDREC  DS    CL1024              UPLOAD RECORD                                
*                                                                               
         DS    0D                  ALIGNMENT                                    
SRTREC   DS    0CL332              SORT RECORD                                  
*                                                                               
SRTKEY   DS    0X                  SORT KEY                                     
SRTTYPE  DS    XL1                 NET/SPOT INDICATOR                           
SRTAGY   DS    CL6                 AGENCY CODE                                  
SRTCMID  DS    CL12                COMMERCIAL ID                                
SRTCLCD  DS    CL3                 CLIENT CODE                                  
SRTPRCD  DS    CL3                 PRODUCT CODE                                 
SRTMDCD  DS    CL1                 MEDIA CODE                                   
SRTRECID DS    CL3                 RECORD ID                                    
SRTDATE  DS    CL3                 DATE - PWOS                                  
*                                                                               
SRTKEYLN EQU   *-SRTKEY            LENGTH OF SORT KEY                           
*                                                                               
SRTCMLN  DS    XL1                 COMMERCIAL LENGTH                            
SRTFLNM  DS    CL36                UNIQUE FILE NAME                             
         DS    CL(96-(*-SRTKEY))   SPARE                                        
*                                                                               
SRTHDRLN EQU   *-SRTKEY            LENGTH OF SORT HEADER                        
*                                                                               
SRTDATA  DS    CL200               DATA FOR SORT                                
*                                                                               
SRTRECLN EQU   *-SRTREC            SORT RECORD LENGTH                           
*                                                                               
MYWORK   DS    CL400               WORKER FILE AREA                             
         ORG                                                                    
*                                                                               
*        TABLE FOR AGENY CODES                                                  
*                                                                               
TABAGYTB DS    100XL(TAGYLQ)       100 ENTRIES IN TABLE                         
MYDLNQ   EQU   *-MYD                                                            
*                                                                               
         TITLE 'T70365 - NON-DDS CLARUS - PRNTD'                                
***********************************************************************         
*                                                                     *         
*        PRINT LINE DSECT                                             *         
*                                                                     *         
***********************************************************************         
                                                                                
PRNTD    DSECT                                                                  
BL       DS    C                                                                
PRTNID   DS    CL(L'SRTCMID)       NETWORK COMMERCIAL ID                        
         DS    CL1                                                              
PRTCONT  DS    CL11                                                             
PRTWARN  DS    CL18                                                             
BC1      DS    C                                                                
PRTNLEN  DS    CL3                 NID LENGTH                                   
BC2      DS    C                                                                
*                                  (MATCHED FOR NETWORK)                        
PRTCID   DS    CL(L'TGCID)         TALENT COMMERCIAL ID                         
         DS    CL24                                                             
BC3      DS    C                                                                
PRTMETHD DS    CL8                 METHOD MATCHED                               
BC4      DS    C                                                                
PRTUSED  DS    CL8                 USE DATE                                     
BC5      DS    C                                                                
PRTPGM   DS    CL(L'TANPPNME)      PROGRAM NAME                                 
BC6      DS    C                                                                
         DS    CL1                                                              
PRTNWK   DS    CL1                 NETWORK                                      
         DS    CL1                                                              
BR       DS    C                                                                
         ORG   PRTPGM              (MATCHED FOR CABLE)                          
PRTCNTI  DS    CL4                                                              
         ORG   PRTCNTI                                                          
PRTCMKT  DS    CL4                                                              
         ORG   PRTCMKT                                                          
PRTCSYS  DS    CL6                                                              
BC6C     DS    C                                                                
         DS    C                                                                
PRTCUSE  DS    CL3                                                              
         DS    C                                                                
BRC      DS    C                                                                
         ORG   PRTCID              (UNMATCHED FOR NETWORK)                      
PRTUUSED DS    CL8                 USED DATE                                    
UBC3     DS    C                                                                
PRTUPGM  DS    CL15                PROGRAM                                      
UBC4     DS    C                                                                
         DS    CL1                                                              
PRTUNWK  DS    CL1                 NETWORK                                      
         DS    CL1                                                              
UBC5     DS    C                                                                
PRTCHG   DS    CL30                CHANGE DESCRIPTION                           
UBR      DS    C                                                                
         ORG   PRTUPGM             (UNMATCHED FOR CABLE)                        
PRTUCNTI DS    CL4                 NTI CODE                                     
         ORG   PRTUPGM                                                          
PRTUCMKT DS    CL4                 MARKET CODE                                  
         ORG   PRTUPGM                                                          
PRTUCSYS DS    CL6                 CABLE SYSTEM                                 
UBC4C    DS    C                                                                
         DS    C                                                                
PRTUCUSE DS    CL3                                                              
         DS    C                                                                
UBC6C    DS    C                                                                
PRTCCHG  DS    CL30                CHANGE DESCRIPTION                           
UBRC     DS    C                                                                
         EJECT                                                                  
*              DSECT FOR SECOND PRINT LINE                                      
         SPACE 1                                                                
PRNT2D   DSECT                                                                  
         DS    C                                                                
PRTNIDN  DS    CL38                NID TITLE                                    
         DS    CL5                                                              
PRTCIDN  DS    CL36                TALENT COMMERCIAL TITLE                      
*                                                                               
         ORG   PRTCIDN             (MATCHED REPORT FOR SPOT)                    
PRTCIDNS DS    CL25                TALENT COMMERCIAL TITLE                      
*                                                                               
*        UNMATCHED WILDSPOT PRINT LINE                                          
*                                                                               
PRWD     DSECT                                                                  
PRWBL    DS    C                                                                
PRWNID   DS    CL12                COMMERCIAL ID                                
         DS    CL1                                                              
PRWTTL   DS    CL36                COMMERCIAL TITLE                             
PRWBC1   DS    C                                                                
PRWNLEN  DS    CL3                 NID LENGTH                                   
PRWBC2   DS    C                                                                
PRWUSED  DS    CL8                 USE DATE                                     
PRWBC3   DS    C                                                                
PRWMKTCD DS    CL4                 MARKET CODE                                  
         DS    CL1                                                              
PRWMKTNM DS    CL15                                                             
PRWBRC   DS    C                                                                
         TITLE 'T70365 - NON-DDS CLARUS - TABAGYD'                              
***********************************************************************         
*                                                                     *         
*        DSECT FOR AGENCY TABLE                                       *         
*                                                                     *         
***********************************************************************         
                                                                                
TABAGYD  DSECT                                                                  
*                                                                               
TAGYENT  DS    0X                  ENTRY IN AGENCY TABLE                        
TAGYAGY  DS    CL6                 AGENCY CODE                                  
TAGYNAME DS    CL36                AGENCY NAME                                  
*                                                                               
TAGYLQ   EQU   *-TAGYENT           LENGTH OF ENTRY IN TABLE                     
*                                                                               
         EJECT                                                                  
       ++INCLUDE DMWRKRK                                                        
         EJECT                                                                  
       ++INCLUDE DMWRKRD                                                        
         EJECT                                                                  
       ++INCLUDE DDSMTPD                                                        
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPE2D                                                       
         EJECT                                                                  
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*DDPERVALD                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*CTGENFILE                                                                      
*DDDLCB                                                                         
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDLCB                                                         
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017TAREP65   09/03/19'                                      
         END                                                                    
