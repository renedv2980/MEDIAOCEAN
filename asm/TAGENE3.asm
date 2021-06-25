*          DATA SET TAGENE3    AT LEVEL 011 AS OF 02/26/16                      
*PHASE T702E3C,*                                                                
         TITLE 'T702E3 - JOB MAINTENANCE'                                       
T702E3   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TMPLNQ,T702E3,R5                                                 
         LR    R6,RC               R6=A(TEMPORARY STORAGE)                      
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=PROGRAM STORAGE                           
         USING JOBD,R7                                                          
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         CLI   PFAID,13            IF PF13 PRESSED TO RETURN                    
         BNE   JB3                                                              
         BAS   RE,CLRELINF         CLEAR ELEMENT INFO                           
         OC    TGJWBUNT,TGJWBUNT   JWT AGENCY, DON'T VAL CLI / PRD              
         BNZ   JB2                                                              
         TM    TGAYSTA7,TAAYSBBD   BBDO JOB AGY, DON'T VAL CLI / PRD            
         BZ    JB3                                                              
JB2      MVC   TGAGY,SJBAGY                                                     
*                                                                               
JB3      GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
*                                                                               
         ST    R6,ATMPAREA         SAVE R6 IN WORKING STORAGE                   
         LA    R1,WRKIO                                                         
         ST    R1,AWRKIO                                                        
*                                                                               
**NO-OP**CLI   MODE,SETFILE        IF MODE IS SET ALTERNATE FILE                
*        BNE   JB5                                                              
*        CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
*        BNE   XIT                                                              
*        CLI   THISLSEL,CHASELQ    AND SELECTED FOR CHANGE                      
*        BNE   XIT                                                              
*        CLC   THISPG,NPAGES       IF DISPLAYED ALL PAGES                       
*        BE    XIT                                                              
*        L     R1,SYSPARMS                                                      
*        L     R1,0(R1)                                                         
*        USING TIOBD,R1            R1=A(TRANSLATOR I/O BLOCK)                   
*        LA    RF,SJBJBSH                                                       
*        S     RF,ATWA                                                          
*        STH   RF,TIOBLAST         FORCE GENCON TO THINK A FLD WAS I/P          
**NO-OP**B     XIT                                                              
*                                                                               
JB5      CLI   MODE,VALKEY         IF MODE IS VALKEY                            
         BNE   *+12                                                             
         BAS   RE,VKEY             VALIDATE KEY                                 
         B     XIT                                                              
*                                                                               
         CLI   MODE,DISPKEY        IF MODE IS DISPLAY KEY                       
         BNE   *+12                                                             
         BAS   RE,DKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,VALREC         IF MODE IS VALIDATE RECORD                   
         BNE   JB10                                                             
         CLI   ACTNUM,ACTDIS       IF REALLY DISPLAY                            
         BE    JB20                GO DISPLAY IT                                
         CLI   ACTNUM,ACTSEL       IF SELECT FOR CHANGE FROM LIST               
         BNE   *+14                                                             
         MVC   JOBKEY,KEY          GET FULL RECORD INTO IOAREA                  
         BRAS  RE,GETJBREC                                                      
         BAS   RE,BLDREC           AND BUILD THE RECORD                         
         B     XIT                                                              
*                                                                               
JB10     CLI   MODE,XRECDEL        IF RECORD 'DELETED'                          
         BNE   JB13                                                             
         TM    TGAYSTA7,TAAYSBBD                                                
         BO    XIT                                                              
         OI    DMINBTS,X'08'       SET TO READ DELETED                          
         BAS   RE,DELETE           DELETE REMAINING RECORDS                     
*                                                                               
JB13     CLI   MODE,XRECREST       OR 'RESTORED'                                
         BNE   *+8                                                              
         BAS   RE,RESTRECS         RESTORE REMAINING RECORDS                    
*                                                                               
         CLI   MODE,RECPUT         IF ABOUT TO WRITE A CHANGE                   
         BNE   *+8                                                              
         MVI   IOOPT,C'Y'          SET SO CONTROLLER DOESN'T DO WRITE           
*                                                                               
         CLI   MODE,XRECPUT        IF JUST WROTE A RECORD                       
         BNE   *+8                                                              
         MVI   IOOPT,C'N'          RESET I/O SWITCH                             
*                                                                               
         CLI   MODE,XRECADD        IF JUST RECORD ADDED                         
         BE    JB20                                                             
         CLI   MODE,XRECDEL        OR DELETED                                   
         BE    JB20                                                             
         CLI   MODE,XRECREST       OR RESTORED                                  
         BE    JB20                                                             
         CLI   MODE,XRECPUT        OR CHANGED                                   
         BE    JB20                                                             
         CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BNE   XIT                                                              
         CLI   THISLSEL,C'D'       AND NOT SELECTED FOR DELETION                
         BE    XIT                                                              
JB20     BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         TM    TGAYSTA7,TAAYSBBD                                                
         BO    XIT                                                              
         CLI   ACTNUM,ACTDIS       IF ACTION IS DISPLAY                         
         BE    PGDSPMSG            GIVE MY OWN MESSAGE                          
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         BE    PGDSPMSG            GIVE MY OWN MESSAGE                          
         CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BNE   XIT                                                              
         CLC   THISPG,NPAGES       AND NOT LAST PAGE                            
         BNE   PGDSPMSG            GIVE MY OWN MESSAGE                          
         B     XIT                                                              
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
         CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BE    *+8                                                              
         NI    SJBAGYH+4,X'DF'     SET AGENCY FIELD CHANGED                     
*                                                                               
         NI    GENSTAT4,X'FF'-NODELLST                                          
         TM    TGAYSTA7,TAAYSBBD                                                
         BZ    VK03                                                             
         NI    SJBAGYH+4,X'DF'     SET AGENCY FIELD CHANGED                     
         OI    GENSTAT4,NODELLST                                                
*                                                                               
VK03     LA    R2,SJBAGYH          IF AGENCY CHANGED                            
         TM    4(R2),X'20'                                                      
         BO    VK10                                                             
         NI    SJBDTEH+4,X'DF'     SET TO VALIDATE NEXT FIELD                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SJBAGYNH                        
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL            GET AGENCY ELEMENT                           
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
         MVC   TGJWBUNT,TAAYBUNT                                                
*                                                                               
         MVC   SJBBUNH,=C'Bus. Unit'                                            
         OI    SJBBUNHH+1,X'0C'    HIDE BUS UNIT FIELD                          
         OI    SJBBUNTH+1,X'2C'                                                 
         NI    SJBCLHDH+1,X'F3'    SHOW CLI AND PROD FIELDS                     
         NI    SJBCLIH+1,X'D3'                                                  
         NI    SJBPRHDH+1,X'F3'                                                 
         NI    SJBPRDH+1,X'D3'                                                  
         NI    SJBJBHDH+1,X'F3'                                                 
         NI    SJBJOBH+1,X'D3'                                                  
*                                                                               
         OC    TGJWBUNT,TGJWBUNT   NOT JWT AGENCY                               
         BZ    VK05                                                             
         NI    SJBBUNHH+1,X'F3'    SHOW BUS UNIT FIELD                          
         NI    SJBBUNTH+1,X'D3'                                                 
         B     VK10                                                             
*                                                                               
VK05     TM    TGAYSTA7,TAAYSBBD                                                
         BZ    VK07                                                             
         MVC   SJBBUNH,=C'BBDO  Job'                                            
         NI    SJBBUNHH+1,X'F3'    SHOW BUS UNIT FIELD                          
         NI    SJBBUNTH+1,X'D3'                                                 
         OI    SJBCLHDH+1,X'0C'    HIDE CLI AND PROD FIELDS                     
         OI    SJBCLIH+1,X'2C'                                                  
         OI    SJBPRHDH+1,X'0C'                                                 
         OI    SJBPRDH+1,X'2C'                                                  
         OI    SJBJBHDH+1,X'0C'    HIDE START JOB                               
         OI    SJBJOBH+1,X'2C'                                                  
         CLI   ACTNUM,ACTADD       IF ADDING RECORD                             
         BE    VK06                                                             
         CLI   ACTNUM,ACTCHA       IF CHANGING RECORD                           
         BE    VK06                                                             
         CLI   ACTNUM,ACTDEL       OR DELETING RECORD                           
         BNE   VK10                                                             
VK06     B     BDJINVAC                                                         
*                                                                               
VK07     BAS   RE,GETSETUP         GET ESTIMATE SETUP LENGTHS                   
VK10     OI    SJBBUNHH+6,X'80'                                                 
         OI    SJBBUNTH+6,X'80'                                                 
         OI    SJBCLHDH+6,X'80'                                                 
         OI    SJBCLIH+6,X'80'                                                  
         OI    SJBPRHDH+6,X'80'                                                 
         OI    SJBPRDH+6,X'80'                                                  
         OI    SJBJBHDH+6,X'80'                                                 
         OI    SJBJOBH+6,X'80'                                                  
*                                                                               
         LA    R2,SJBDTEH          R2=A(DATE FIELD)                             
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    VK20                                                             
         NI    SJBCLIH+4,X'DF'     SET TO VALIDATE NEXT FIELD                   
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VK15                                                             
         OC    TGDATE,TGDATE       TRY GLOBAL DATE                              
         BZ    VK12                                                             
         CLI   TGDATE+1,C'A'       IF DATE COMPLEMENTED                         
         BL    *+10                                                             
         XC    TGDATE,HEXFFS       UNCOMPLEMENT IT                              
         GOTO1 DATCON,DMCB,(1,TGDATE),(8,8(R2))                                 
         B     *+10                                                             
VK12     MVC   8(8,R2),TGTODAY8    ELSE, USE TODAY'S DATE                       
         MVI   5(R2),8                                                          
         OI    6(R2),X'80'                                                      
VK15     GOTO1 DTVAL,DMCB,TGDATE                                                
*                                                                               
VK20     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         OC    TGJWBUNT,TGJWBUNT   JWT AGENCY, DON'T VAL CLI / PRD              
         BNZ   VK80                                                             
         TM    TGAYSTA7,TAAYSBBD   SAME FOR BBDO JOB AGY                        
         BO    VK80                                                             
         LA    R2,SJBCLIH          R2=A(CLIENT FIELD)                           
         TM    4(R2),X'20'                                                      
         BO    VK30                                                             
         NI    SJBPRDH+4,X'DF'     SET TO VALIDATE NEXT FIELD                   
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'0C',(R2)),SJBCLINH                        
*                                                                               
VK30     LA    R2,SJBPRDH          R2=A(PRODUCT FIELD)                          
         TM    4(R2),X'20'                                                      
         BO    VK40                                                             
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'0C',(R2)),SJBPRDNH                        
*                                                                               
VK40     MVC   STRTJOB,SJBJOB      SAVE STARTING JOB                            
         CLI   ACTNUM,ACTADD       IF ADDING RECORD                             
         BNE   VK45                                                             
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLJBD,R4                                                         
         MVI   TLJBCD,TLJBCDQ                                                   
         MVC   TLJBAGY,TGAGY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLJBDTE-TLJBD),KEYSAVE  AND NO RECS FOR THIS AGENCY          
         BNE   VK45                        ACCEPT ANY DATE                      
         MVC   WORK,TLJBDTE                ELSE, MUST BE LATEST DATE            
         XC    WORK,HEXFFS                                                      
         CLC   TGDATE,WORK                                                      
         BE    VK45                                                             
         MVC   TGDATE,WORK                                                      
         GOTO1 DATCON,DMCB,(1,TGDATE),(8,SJBDTE)                                
         MVI   SJBDTEH+5,8                                                      
         OI    SJBDTEH+6,X'80'                                                  
*                                                                               
VK45     GOTO1 RECVAL,DMCB,TLJBCDQ,(X'40',0) BUILD THE KEY                      
         MVC   JOBKEY,KEY                                                       
         GOTO1 HIGH                IF KEY EXISTS                                
         CLC   KEY(L'JOBKEY),JOBKEY                                             
         BE    VK50                                                             
         BAS   RE,SETJBREC         SET NEW RECORD                               
         CLI   ACTNUM,ACTADD       IT ISN'T - IF THIS ISN'T ACTION ADD          
         BE    VKX                                                              
         CLI   ACTNUM,ACTREST      OR RESTORE                                   
         BNE   RECNTFND            THEN FLAG NO SUCH RECORD                     
         NI    WHENOK,X'FE'        INSURE CONTROLLER HANDLES MAINT.             
         B     VKX                                                              
*                                                                               
VK50     CLI   ACTNUM,ACTADD       IT IS - IF THIS IS ACTION ADD                
         BE    RECONFIL            FLAG DUPLICATE                               
         BRAS  RE,GETJBREC         GET FULL RECORD                              
         CLI   ACTNUM,ACTDEL       IF WE'RE DELETING                            
         BNE   VKX                                                              
         NI    WHENOK,X'FE'        INSURE CONTROLLER HANDLES MAINT.             
         B     VKX                                                              
*                                                                               
VK80     TM    TGAYSTA7,TAAYSBBD                                                
         BZ    VKX                                                              
         LA    R4,KEY                                                           
         LA    R2,SJBBUNTH                                                      
         MVC   JOBKEY,KEY                                                       
         XC    KEY,KEY                                                          
         MVI   TLJBCD,TLJBCDQ                                                   
         MVI   TLJBSPCL,TLJBSPBD   BBDO JOBS                                    
         MVC   TLJBPROJ,SJBBUNT                                                 
         OC    TLJBPROJ,SPACES                                                  
         GOTO1 HIGH                IF KEY EXISTS                                
         CLC   KEY(TLJBDAT-TLJBD),KEYSAVE                                       
         BNE   RECNTFND                                                         
         GOTO1 GETREC                                                           
         MVC   SJBJBS(L'TLJBPROJ),TLJBPROJ                                      
         OI    SJBJBSH+6,X'80'                                                  
*                                                                               
VKX      MVC   KEY,JOBKEY          RESTORE KEY FOR CONTROLLER                   
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO SEE IF ANY OTHER RECORDS NEED TO BE RESTORED          
         SPACE 1                                                                
RESTRECS NTR1                                                                   
*                                                                               
RESTREC5 L     R6,AIO              R6=A(RECORD JUST RESTORED)                   
         USING TLJBD,R6                                                         
*                                                                               
         MVI   ELCODE,TAACELQ      LOOK FOR ACTIVITY EL.                        
         BAS   RE,GETEL                                                         
         BE    XIT                 FOUND IT, SO NOTHING MORE TO RESTORE         
*                                                                               
         GOTO1 HIGH                RE-READ DIRECTORY REC. JUST RESTORED         
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'       SET READ FOR DELETED                         
         GOTO1 SEQ                 CHECK FOR ANOTHER RECORD                     
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(TLJBSEQ-TLJBD),KEYSAVE  TEST STILL RECORD                    
         BNE   XIT                                                              
         OI    DMINBTS,X'08'       SET READ FOR DELETED                         
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET THE RECORD                               
         NI    TLJBSTAT,X'7F'      UNMARK IT                                    
         GOTO1 PUTREC              AND WRITE IT BACK                            
         NI    KEY+TLDRSTAT-TLDRD,X'7F'  UNMARK DIRECTORY RECORD                
         GOTO1 WRITE                     AND WRITE IT BACK                      
         B     RESTREC5            LOOK FOR MORE                                
         SPACE 2                                                                
*              ROUTINE TO ESTABLISH NEW RECORD                                  
         SPACE 1                                                                
SETJBREC NTR1                                                                   
         L     R6,AIO              INITIALIZE RECORD                            
         USING TLJBD,R6                                                         
         MVC   TLJBKEY,JOBKEY                                                   
         MVC   TLJBLEN,DATADISP                                                 
         XC    TLJBSTAT(10),TLJBSTAT                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY THE KEY                                       
         SPACE 1                                                                
DKEY     NTR1                                                                   
         CLI   ACTNUM,ACTSEL       IF SELECT FOR CHANGE FROM LIST               
         BNE   *+10                                                             
         XC    STRTJOB,STRTJOB     NEED TO CLEAR IF SEL FROM LIST               
         CLC   SVKEY,KEY           IF KEY CHANGE                                
         BE    *+8                                                              
         BAS   RE,CLRELINF         CLEAR LAST EL. KEYS                          
         MVC   SVKEY,KEY           SAVE KEY                                     
*                                                                               
         L     R6,AIO1             R6=A(JOB RECORD)                             
         USING TLJBD,R6                                                         
         MVC   JOBKEY,TLJBKEY                                                   
         BRAS  RE,GETJBREC         GET FULL RECORD                              
*                                                                               
         OC    TGJWBUNT,TGJWBUNT   JWT AGENCY                                   
         BNZ   DKEY100                                                          
         TM    TGAYSTA7,TAAYSBBD   BBDO JOB AGENCY                              
         BO    DKEY105                                                          
*                                                                               
         LA    R2,SJBAGYH          DISPLAY AGENCY                               
         MVC   8(L'TLJBAGY,R2),TLJBAGY                                          
         MVI   5(R2),L'TLJBAGY                                                  
         OI    6(R2),X'80'                                                      
         MVC   AIO,AWRKIO                                                       
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SJBAGYNH                        
         BAS   RE,GETSETUP         GET ESTIMATE SETUP LENGTHS                   
         MVC   AIO,AIO1                                                         
*                                                                               
DKEY100  OC    TGJWBUNT,TGJWBUNT   JWT AGENCY                                   
         BZ    DKEY110                                                          
         MVC   SJBCLI,TLJBCSTI                                                  
         OI    SJBCLIH+6,X'80'                                                  
         MVC   SJBPRD,TLJBPRDI     PRODUCT                                      
         OI    SJBPRDH+6,X'80'                                                  
         MVC   SJBJBS(L'TLJBPRJI),TLJBPRJI                                      
         OI    SJBJBSH+6,X'80'                                                  
         MVC   WORK(3),TLJBDATE                                                 
         B     DKEY150                                                          
*                                                                               
DKEY105  MVC   SJBJBS(L'TLJBPROJ),TLJBPROJ                                      
         OI    SJBJBSH+6,X'80'                                                  
         MVC   WORK(3),TLJBDAT                                                  
*                                                                               
         B     DKEY150                                                          
*                                                                               
DKEY110  MVC   WORK(3),TLJBDTE     DATE                                         
DKEY150  XC    WORK(3),HEXFFS      UNCOMPLEMENT                                 
         MVC   TGDATE,WORK                                                      
         GOTO1 DATCON,DMCB,(1,WORK),(8,SJBDTE)                                  
         OI    SJBDTEH+6,X'80'                                                  
*                                                                               
         OC    TGJWBUNT,TGJWBUNT   JWT AGENCY                                   
         BZ    DKEY170                                                          
         NI    SJBBUNHH+1,X'F3'    YES, SHOW BUS UNIT FIELDS                    
         NI    SJBBUNTH+1,X'D3'                                                 
         NI    SJBCLHDH+1,X'F3'    SHOW CLI AND PROD FIELDS                     
         NI    SJBCLIH+1,X'D3'                                                  
         NI    SJBPRHDH+1,X'F3'                                                 
         NI    SJBPRDH+1,X'D3'                                                  
         MVC   SJBBUNT,TLJBBUNT                                                 
         B     DKEY200                                                          
*                                                                               
DKEY170  TM    TGAYSTA7,TAAYSBBD                                                
         BZ    DKEY180                                                          
         MVC   SJBBUNH,=C'BBDO  Job'                                            
         NI    SJBBUNHH+1,X'F3'    SHOW BUS UNIT FIELDS                         
         NI    SJBBUNTH+1,X'D3'                                                 
         OI    SJBCLHDH+1,X'0C'    HIDE CLI AND PROD FIELDS                     
         OI    SJBCLIH+1,X'2C'                                                  
         OI    SJBPRHDH+1,X'0C'                                                 
         OI    SJBPRDH+1,X'2C'                                                  
         OI    SJBJBHDH+1,X'0C'    HIDE START JOB                               
         OI    SJBJOBH+1,X'2C'                                                  
         MVC   SJBBUNT,TLJBPROJ                                                 
         B     DKEY200                                                          
*                                                                               
DKEY180  OI    SJBBUNHH+1,X'0C'    HIDE BUS UNIT FIELDS                         
         OI    SJBBUNTH+1,X'2C'                                                 
         NI    SJBCLHDH+1,X'F3'    SHOW CLI AND PROD FIELDS                     
         NI    SJBCLIH+1,X'D3'                                                  
         NI    SJBPRHDH+1,X'F3'                                                 
         NI    SJBPRDH+1,X'D3'                                                  
*                                                                               
         MVC   SJBCLI,TLJBCLI      CLIENT                                       
         MVI   SJBCLIH+5,L'TLJBCLI                                              
         OI    SJBCLIH+6,X'80'                                                  
         MVC   AIO,AWRKIO                                                       
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'0C',SJBCLIH),SJBCLINH                     
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   SJBPRD,TLJBPRD      PRODUCT                                      
         MVI   SJBPRDH+5,L'TLJBPRD                                              
         OI    SJBPRDH+6,X'80'                                                  
         MVC   AIO,AWRKIO                                                       
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'0C',SJBPRDH),SJBPRDNH                     
         MVC   AIO,AIO1                                                         
*                                                                               
DKEY200  OI    SJBBUNHH+6,X'80'                                                 
         OI    SJBBUNTH+6,X'80'                                                 
         OI    SJBCLHDH+6,X'80'                                                 
         OI    SJBCLIH+6,X'80'                                                  
         OI    SJBPRHDH+6,X'80'                                                 
         OI    SJBPRDH+6,X'80'                                                  
         OI    SJBJBHDH+6,X'80'                                                 
         OI    SJBJOBH+6,X'80'                                                  
         MVC   KEY,SVKEY           RESTORE KEY                                  
                                                                                
         TM    TGAYSTA7,TAAYSBBD                                                
         BZ    XIT                                                              
         CLI   THISLSEL,C'C'                                                    
         BE    BDJINVAC                                                         
         CLI   THISLSEL,C'D'                                                    
         BE    BDJINVAC                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET ESTIMATE LENGTHS FROM AGENCY                      
         SPACE 1                                                                
GETSETUP NTR1                                                                   
         XC    LENGTHS,LENGTHS                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTJOB))                                     
         BNE   FLDINV                                                           
         L     R4,TGELEM                                                        
         USING TANUD,R4                                                         
         MVC   LENGTHS,TANUMBER                                                 
         NI    LENGTHS,X'0F'       TURN OFF CHARACTERS                          
         NI    LENGTHS+1,X'0F'                                                  
         NI    LENGTHS+2,X'0F'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         TM    TGAYSTA7,TAAYSBBD                                                
         BO    BDJINVAC                                                         
         MVC   SVKEY,KEY           SAVE THE KEY                                 
         L     R6,AIO1             R6=A(JOB RECORD)                             
         USING TLJBD,R6                                                         
         CLI   ACTNUM,ACTCHA       IF ACTION CHANGE                             
         BNE   BLDREC5                                                          
         MVC   JOBKEY,TLJBKEY      MUST REREAD                                  
         BRAS  RE,GETJBREC                                                      
*                                                                               
BLDREC5  BAS   RE,VALJBS           VALIDATE JOBS                                
*                                                                               
         GOTO1 ACTVIN,DMCB,(X'40',0) REMOVE EXISTING ACTIVITY ELEMENT           
         LA    R4,ELEMENT          AND BUILDS NEW ONE                           
         BAS   RE,MYADDL                                                        
*                                                                               
         BAS   RE,WRITEIT          WRITE BACK THE RECORD(S)                     
*                                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE JOBS                                     
         SPACE 1                                                                
VALJBS   NTR1                                                                   
         OC    JBS,JBS             IF SOMETHING ON SCREEN                       
         BZ    VALJBS30                                                         
*                                                                               
         L     R4,AIO              SET TO DELETE CORRES ELEMENTS                
         USING TAGLD,R4            R4=A(ELEMENT)                                
         MVI   ELCODE,TAGLELQ                                                   
         BAS   RE,GETEL                                                         
         BE    VALJBS10                                                         
         B     VALJBS30                                                         
*        DC    H'0'                                                             
*                                                                               
VALJBS10 CLC   TAGLDATA(L'JBSFRST),JBSFRST IF ELEMENT W/IN RANGE                
         BL    VALJBS20                                                         
         CLC   TAGLDATA(L'JBSLAST),JBSLAST                                      
         BH    VALJBS25                                                         
         MVI   TAGLEL,X'FF'               SET TO DELETE                         
*                                                                               
VALJBS20 BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    VALJBS10                                                         
*                                                                               
VALJBS25 MVI   ELCODE,X'FF'        REMOVE MARKED ELEMENTS                       
         GOTO1 REMELEM                                                          
*                                                                               
VALJBS30 LA    R2,SJBJBSH          R2=A(FIRST LINE)                             
         GOTO1 ANY                                                              
*                                                                               
VALJBS40 CLI   5(R2),0             IF BLANK LINE                                
         BE    VALJBS90            SKIP LINE                                    
         MVI   ERRDISP,0                                                        
         GOTO1 SCANNER,DMCB,(R2),(X'80',AWRKIO)                                 
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            R0=N'ENTRIES IN BLOCK                        
         L     R3,AWRKIO           R3=A(BLOCK)                                  
         USING SCAND,R3                                                         
         SPACE 1                                                                
VALJBS50 MVC   ERRDISP,SCDISP1     SET DISP. TO THIS DATA                       
         CLC   SCLEN1,LENGTHS+2    SIZE OF JOB                                  
         BNE   FLDINV                                                           
         CLI   SCLEN2,0            SHOULD BE NO RHS                             
         BNE   FLDINV                                                           
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAGLD,R4                                                         
         MVI   TAGLEL,TAGLELQ                                                   
         MVI   TAGLLEN,TAGLLNQ+6                                                
         MVC   TAGLDATA(6),SPACES                                               
*                                                                               
         ZIC   R1,SCLEN1           SET FOR EXECUTED MOVE                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TAGLDATA(0),SCDATA1 MOVE DATA TO ELEMENT                         
*                                                                               
         MVI   ELCODE,TAGLELQ      BEFORE ADDING CHECK FOR DUP JOB              
         LA    R4,TAGLLEN                                                       
         LA    R4,1(R4)                                                         
         GOTO1 GETL,DMCB,(6,(R4))                                               
         BE    FLDINV                                                           
         BAS   RE,MYADDL           ADD THE ELEMENT                              
         LA    R3,SCANNEXT                                                      
         BCT   R0,VALJBS50                                                      
         SPACE 1                                                                
VALJBS90 ZIC   R1,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R1                                                            
         LA    RF,SJBJLSTH         IF NOT END OF SCREEN                         
         CR    R2,RF                                                            
         BNH   VALJBS40            KEEP PROCESSING                              
         MVI   ERRDISP,0           CLEAR DISPLACEMENT INTO FLD                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO ADD ELEMENT                                           
         SPACE 1                                                                
MYADDL   NTR1                                                                   
         L     R6,AIO                                                           
         LA    R4,ELEMENT                                                       
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R6),(R4),0                             
         CLI   12(R1),5                                                         
         BE    MYADDL10                                                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TLJBD,R6                                                         
MYADDL10 LH    RE,TLJBLEN          CUMULATIVE MAX RECORD SIZE                   
         CH    RE,=H'5700'                                                      
         BH    RCTOOLNG                                                         
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         OC    TGJWBUNT,TGJWBUNT                                                
         BNZ   XIT                                                              
         TM    TGAYSTA7,TAAYSBBD                                                
         BO    XIT                                                              
*                                                                               
         TWAXC SJBJBSH,SJBJLSTH,PROT=Y  CLEAR THE SCREEN                        
         MVC   SVKEY,KEY                SAVE THE KEY                            
*                                                                               
         BAS   RE,DISJBS                DISPLAY THE JOBS                        
*                                                                               
         GOTO1 ACTVOUT,DMCB,SJBLCHGH    AND LAST CHANGED                        
*                                                                               
         GOTO1 FLDVAL,DMCB,(X'20',AFRSTREC),999 AND MAKE ALL FLDS VALID         
*                                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY THE JOBS                                      
         SPACE 1                                                                
DISJBS   NTR1                                                                   
         L     R6,AIO              R6=A(RECORD)                                 
         USING TLJBD,R6                                                         
*                                                                               
         CLI   MODE,XRECPUT        IF JUST CHANGED RECORD                       
         BNE   DISJB30                                                          
         CLI   PFAID,14            AND USER WANTS ROOM FOR MORE                 
         BNE   DISJB30                                                          
         BAS   RE,CLRELINF         CLEAR ELEMENT INFO                           
         LA    R2,SJBJBSH                                                       
         B     XIT                 AND EXIT                                     
*                                                                               
DISJB30  NI    STATUS,X'FF'-NOJOBS                                              
         BAS   RE,SETSTART         SET STARTING POINT                           
         TM    STATUS,NOJOBS                                                    
         BO    DISJBX                                                           
         USING TAGLD,R4            RETURNS R4=A(ELEMENT)                        
*                                                                               
         L     R3,ATMPAREA         R3=A(UNSCAN BLOCK)                           
         LA    R2,SJBJBSH          R2=A(FIRST LINE)                             
         XR    R0,R0               R0=N'ENTRIES                                 
         MVI   JOBCONT,C'N'                                                     
         MVC   JBSFRST,TAGLDATA    SAVE KEY OF FRST EL. DISPLAYED               
*                                                                               
DISJB35  MVC   JBSLAST,TAGLDATA    SAVE KEY OF LAST EL. DISPLAYED               
*                                                                               
         MVC   0(20,R3),SPACES     PRE - CLEAR ENTRY                            
         ZIC   R1,TAGLLEN                                                       
         SH    R1,=AL2(TAGLLNQ+1)                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),TAGLDATA                                                 
         LA    R3,20(R3)                                                        
         AHI   R0,1                                                             
         CH    R0,=AL2(TMPLNQ/20)  IF NO MORE ROOM IN TMPAREA                   
         BL    *+6                                                              
         DC    H'0'                NEED TO INCREASE                             
         BAS   RE,NEXTEL           ELSE, LOOK FOR MORE ELEMENTS                 
         BE    DISJB35                                                          
         MVC   DMCB,ATMPAREA                                                    
         ST    R0,FULL                                                          
         CLC   FULL,=X'000000FF'                                                
         BNH   DISJB40                                                          
         LA    R1,255                                                           
         SR    R0,R1                                                            
         ST    R0,FULL                                                          
         STC   R1,DMCB                                                          
         B     DISJB50                                                          
DISJB40  STC   R0,DMCB             SET N'ENTRIES TO PRINT                       
         XC    FULL,FULL                                                        
*                                                                               
DISJB50  CLI   DMCB,0              TEST ANYTHING (LEFT) TO PRINT                
         BE    DISJBX                                                           
         GOTO1 UNSCAN,DMCB,,(R2),0,0                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         LA    RF,SJBJLSTH         IF NOT PAST END OF SCREEN                    
         CR    R2,RF                                                            
         BNH   DISJB50             KEEP PROCESSING                              
*                                                                               
         L     R3,DMCB             R3=A(NEXT ITEM IN BUFFER)                    
         L     R1,ATMPAREA         IF NOT FIRST ITEM                            
         CR    R3,R1                                                            
         BE    *+8                                                              
         AHI   R3,-20              BACK UP TO PREVIOUS ENTRY                    
         MVC   JBSLAST,0(R3)       SAVE LAST DISPLAYED                          
*                                                                               
DISJBX   CLC   ORIGJOB,SJBJBS      IS THIS FIRST PAGE?                          
         BE    DISJBX1                                                          
         ZIC   R1,THISPG                                                        
         ZIC   R0,NPAGES                                                        
         CR    R1,R0                                                            
         BL    *+12                                                             
DISJBX1  LA    R1,1                                                             
         B     *+8                                                              
         AHI   R1,1                                                             
         STC   R1,THISPG                                                        
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE RETURNS R4=A(CORRECT ELEMENT)                            
         SPACE 1                                                                
SETSTART NTR1                                                                   
*                                                                               
SST10    L     R4,AIO                                                           
         USING TAGLD,R4            R4=A(FIRST ELEMENT)                          
         MVI   ELCODE,TAGLELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    JBS,JBS             IF WE HAVEN'T DISPLAYED ANYTHING YET         
         BNZ   SST15                                                            
         BAS   RE,CNTTPGS          SET NPAGES                                   
         CLC   STRTJOB,SPACES                                                   
         BH    SST60                                                            
         MVC   ORIGJOB,TAGLDATA    STORE VERY FIRST JOB                         
         B     SSTX                AND START WITH FIRST                         
*                                                                               
SST15    XR    R0,R0               INITIALIZE START SWITCH                      
         CLI   MODE,XRECPUT        IF JUST CHANGED RECORD                       
         BE    *+12                                                             
         CLI   MODE,XRECADD        OR JUST ADDED IT                             
         BNE   SST20                                                            
         GOTO1 FLDVAL,DMCB,(X'40',SJBJBSH),SJBJLSTH AND SCREEN CHANGED          
         BE    SST20                                                            
         LHI   R0,1                SET START SWITCH TO REDISPLAY                
*                                                                               
SST20    LTR   R0,R0               IF NEED TO DISPLAY CURRENT PAGE              
         BZ    SST40                                                            
SST35    CLC   TAGLDATA(6),JBSFRST SCAN FOR FIRST ELE DISPLAYED                 
         BNL   SSTX                                                             
         BAS   RE,NEXTEL                                                        
         BE    SST35                                                            
         B     SST50                                                            
*                                                                               
SST40    CLC   TAGLDATA(6),JBSLAST  SCAN FOR 1ST EL AFTER LAST DISP             
         BH    SSTX                                                             
         BAS   RE,NEXTEL                                                        
         BE    SST40                                                            
*                                                                               
SST50    DS    0H                  NONE LEFT - START AT BEGINNING               
         BAS   RE,CLRELINF         CLEAR ELEMENT INFO                           
         B     SST10                                                            
*                                                                               
SST60    DS    0H                  GET FIRST JOB REQUESTED                      
         CLC   TAGLDATA(6),STRTJOB                                              
         BL    *+14                                                             
         MVC   ORIGJOB,TAGLDATA                                                 
         B     SSTX                                                             
         BAS   RE,NEXTEL                                                        
         BE    SST60                                                            
         OI    STATUS,NOJOBS                                                    
*                                                                               
SSTX     XIT1  REGS=(R4)           RETURN R4=A(ELEMENT)                         
         EJECT                                                                  
*              ROUTINE TO CALCULATE TOTAL NUMBER OF PAGES                       
         SPACE                                                                  
CNTTPGS  NTR1                                                                   
         BAS   RE,CNTJBS           COUNT TOTAL NUMBER IN RECORD                 
         LH    R3,HALF                                                          
*                                                                               
         BAS   RE,CALCPGS          CALCULATE N'PAGES FROM ELE COUNT             
         MVC   NPAGES,BYTE                                                      
         B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
*              ROUTINE TO CALCULATE NUMBER OF PAGES                             
*                                  NTRY (R3)= NUMBER OF ELEMENTS                
*                                  XIT BYTE = NUMBER OF PAGES                   
         SPACE                                                                  
CALCPGS  NTR1                                                                   
         ZIC   R2,LENGTHS+2        L'JOB                                        
         LA    R2,1(R2)                                                         
         XR    R0,R0                                                            
         LA    R1,L'SJBJBS         L'LINE                                       
         DR    R0,R2               CALC MAX # OF JOBS PER LINE                  
         MHI   R1,14               * N'LINES                                    
         LR    R2,R1                                                            
         LR    R1,R3               N'JOBS ON RECORD                             
         XR    R0,R0                                                            
         DR    R0,R2               DIVIDE BY N'JOBS/SCREEN                      
         LTR   R0,R0               ANY REMAINDER?                               
         BZ    CALCP10                                                          
         LA    R1,1(R1)            NEED ANOTHER PAGE                            
CALCP10  LTR   R1,R1               HAS TO BE AT LEAST ONE PAGE                  
         BNZ   *+8                                                              
         LA    R1,1                                                             
         STC   R1,BYTE             RETURN BYTE=PAGE NUMBER                      
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO CALCULATE TOTAL NUMBER JOB ELEMENTS                   
*                                  XIT HALF EQUALS NUMBER OF ELEMENTS           
         SPACE                                                                  
CNTJBS   NTR1                                                                   
         XR    R3,R3               R3=COUNT                                     
         USING TAGLD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAGLELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CNTJBS5  BAS   RE,NEXTEL                                                        
         BNE   CNTJBS10                                                         
         CLC   STRTJOB,TAGLDATA                                                 
         BH    CNTJBS5                                                          
         AHI   R3,1                ADD 1 TO COUNT                               
         B     CNTJBS5                                                          
*                                                                               
CNTJBS10 STH   R3,HALF                                                          
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*              ROUTINE TO CLEAR FLAGS FOR LOWER SCREEN                          
         SPACE 1                                                                
CLRELINF NTR1                                                                   
         XC    JBS,JBS             CLEAR LAST EL. KEYS                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO WRITE BACK JOB RECORD(S)                              
         SPACE 1                                                                
WRITEIT  NTR1                                                                   
         MVI   RDUPDATE,C'Y'         SET TO READ FOR UPDATE                     
         NI    GENSTAT1,ALL-RDUPAPPL THIS WILL KEEP IT ON UNTIL END             
         BAS   RE,SPLIT                                                         
         MVI   RDUPDATE,C'N'       TURN OFF READ FOR UPDATE                     
         OI    GENSTAT1,RDUPAPPL   RESET GENCON CONTROL SWITCH                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SPLIT UP ONE LARGE JOB RECORD                         
         SPACE 1                                                                
SPLIT    NTR1                                                                   
         L     R4,AIO1             R4=A(NEW RECORD)                             
         L     R6,AWRKIO                                                        
         ST    R6,AIO              R6=A(RECORD TO BE WRITTEN BACK)              
         USING TLJBD,R6                                                         
         MVC   TLJBKEY,0(R4)       SET KEY IN RECORD                            
         SPACE 1                                                                
         LA    R4,TLJBELEM-TLJBD(R4)  R4=A(FIRST ELEMENT)                       
         MVI   ELCODE,0            SET TO LOOP THROUGH ALL ELEMENTS             
         SPACE 1                                                                
SPL2     MVC   TLJBLEN,DATADISP    INIT. RECORD LENGTH                          
         XC    TLJBSTAT(10),TLJBSTAT     STATUS, BEG. OF REC.                   
         SPACE 1                                                                
SPL4     ZIC   R1,1(R4)            R1 = L'ELEMENT                               
         AH    R1,TLJBLEN             + L'RECORD                                
         CH    R1,=H'1930'         WILL RECORD BECOME TOO LONG                  
         BH    SPL6                YES - GO RELEASE IT                          
         MVC   ELEMENT,0(R4)       NO - MOVE ELEMENT TO W/S                     
         GOTO1 ADDELEM,DMCB,,,,0   AND ADD TO NEW REC.                          
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    SPL4                                                             
         SPACE 1                                                                
SPL6     BAS   RE,RELEASE          WRITE BACK RECORD IN AWRKIO                  
         SPACE 1                                                                
         CLI   0(R4),0             IF THERE ARE MORE ELS. TO PROCESS            
         BNE   SPL2                GO BACK                                      
         SPACE 1                                                                
         BAS   RE,DELETE           DELETE ANY REMAINING RECORDS                 
         MVC   AIO,AIO1            RESTORE IOAREA                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE RELEASES SPLIT-UP ESTIMATE RECORD                        
         SPACE 1                                                                
         USING TLJBD,R6            R6=A(RECORD TO BE WRITTEN BACK)              
RELEASE  NTR1                                                                   
         MVC   KEY,TLJBKEY         MOVE KEY FROM RECORD TO KEY                  
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         GOTO1 HIGH                SEE IF RECORD ALREADY ON FILE                
         SPACE 1                                                                
         LA    R6,KEY              R6=A(DIRECTORY RECORD)                       
         CLC   TLJBKEY,KEYSAVE     IS RECORD ALREADY ON FILE                    
         BE    REL8                                                             
         MVC   TLJBKEY,KEYSAVE     NO, SO RESTORE SAVED KEY                     
         GOTO1 ADDREC              AND ADD NEW RECORD TO FILE                   
         B     RELX                                                             
         SPACE 1                                                                
         USING TLDRD,R6                                                         
REL8     TM    TLDRSTAT,X'80'      IF DIRECTORY MARKED DELETED                  
         BZ    REL10                                                            
         XI    TLDRSTAT,X'80'      UNMARK IT                                    
         GOTO1 WRITE               AND WRITE IT BACK                            
         SPACE 1                                                                
REL10    MVC   AIO,ATIA            RECORD EXISTS - READ IT                      
         GOTO1 GETREC                                                           
         MVC   AIO,AWRKIO          RESET AIO TO A(NEW RECORD)                   
         GOTO1 PUTREC              WRITE BACK NEW FILE RECORD                   
         SPACE 1                                                                
RELX     NI    DMINBTS,X'F7'       TURN OFF READ FOR DELETED                    
         SPACE 1                                                                
         L     R6,AIO              R6=A(RECORD WE JUST ADDED/WROTE)             
         USING TLJBD,R6                                                         
         ZIC   R1,TLJBSEQ          BUMP SEQUENCE NUMBER IN KEY OF REC.          
         LA    R1,2(R1)                                                         
         STC   R1,TLJBSEQ                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SEE IF ANY OTHER RECORDS NEED TO BE DELETED           
         SPACE                                                                  
DELETE   NTR1                                                                   
         TM    TGAYSTA7,TAAYSBBD   DON'T DELETE FOR BDJ AGENCIES                
         BO    XIT                                                              
*                                                                               
         L     R6,AIO              R6=A(FILE RECORD)                            
         USING TLJBD,R6                                                         
*                                                                               
DEL2     GOTO1 HIGH                RE-READ RECORD WE JUST WROTE                 
         NI    DMINBTS,X'F7'       TURN OFF READ DELETED                        
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 SEQ                 GET NEXT                                     
         SPACE 1                                                                
         CLC   KEY(TLJBSEQ-TLJBD),KEYSAVE  TEST STILL SAME RECORD               
         BNE   XIT                                                              
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET THE RECORD                               
         OI    TLJBSTAT,X'80'      MARK IT DELETED                              
         GOTO1 PUTREC              AND WRITE IT BACK                            
         SPACE 1                                                                
         OI    KEY+TLDRSTAT-TLDRD,X'80'  MARK DIRECTORY DELETED                 
         GOTO1 WRITE                     AND WRITE IT BACK                      
         OI    DMINBTS,X'08'       SET TO READ DELETED                          
         B     DEL2                LOOK FOR MORE                                
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 3                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         SPACE 1                                                                
PGDSPMS2 CLI   PFAID,14            TEST DISPLAYED NEW PAGE                      
         BE    PLSENTER                                                         
PGDSPMSG MVI   MYMSGNO1,54         PAGE X OF Y DISPLAYED                        
         L     R2,AFRSTKEY                                                      
         CLI   ACTNUM,ACTDIS                                                    
         BE    PGDSP2                                                           
         CLI   ACTNUM,ACTSEL                                                    
         BNE   *+12                                                             
         CLI   THISLSEL,C'S'                                                    
         BE    PGDSP2                                                           
         MVI   MYMSGNO1,55         .... - ENTER CHANGES AS DESIRED              
PGDSP2   MVI   BLOCK,2                                                          
         EDIT  THISPG,(1,BLOCK+1)                                               
         MVI   BLOCK+2,2                                                        
         EDIT  NPAGES,(1,BLOCK+3)                                               
         MVI   BLOCK+4,0                                                        
         B     INFEND                                                           
*                                                                               
PLSENTER MVI   MYMSGNO1,2          PLEASE ENTER REQUIRED FIELDS                 
         MVI   MYMSYS,X'FF'                                                     
         LA    R2,SJBJBSH                                                       
INFEND   OI    GENSTAT2,USGETTXT                                                
         B     THEEND                                                           
*                                                                               
BDJINVAC MVC   MYMSGNO,=Y(EINVBDJA)   INVALID ACTION FOR BDJ AGENCY             
         MVI   MYMTYP,C'E'                                                      
         NI    SJBAGYH+4,X'DF'     SET AGENCY FIELD CHANGED                     
         B     INFEND                                                           
*                                                                               
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
RECNTFND MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     THEEND                                                           
RECONFIL MVI   ERROR,RECEXIST      RECORD ALREADY ON FILE                       
         B     THEEND                                                           
RCTOOLNG MVI   ERROR,TOOLONG       RECORD TOO LONG                              
         LH    R2,CURDISP          RETURN CURSOR TO SAME SPOT                   
         AR    R2,RA                                                            
         B     THEEND                                                           
*                                                                               
THEEND   GOTO1 EXIT,DMCB,0                                                      
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
HEXFFS   DC    3X'FF'                                                           
         SPACE 2                                                                
PFTAB    DS    0C                  PF KEYS TABLE                                
         SPACE 1                                                                
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'JOB  ',CL8'LIST'                                      
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'JOB  ',CL8'LIST'                                      
PF14X    EQU   *                                                                
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO GET JOB RECORD INTO AIO1,2,&3                         
*---------------------------------------------------------------------          
GETJBREC NTR1  BASE=*,LABEL=*                                                   
         MVC   KEY(L'JOBKEY),JOBKEY  SET KEY                                    
         GOTO1 HIGH                                                             
*                                                                               
         NI    STATUS,X'FF'-HAVEREC                                             
         LA    R6,KEY                                                           
         USING TLJBD,R6                                                         
         CLC   TLJBKEY,KEYSAVE     DID WE GET THE RECORD                        
         BNE   GRNO                                                             
*                                                                               
         MVC   KEYSAVE,TLJBKEY     SAVE ENTIRE DIR. REC. IN KEYSAVE             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              GET RECORD INTO IO1,2, & 3                   
         OI    STATUS,HAVEREC                                                   
         NI    DMINBTS,X'F7'       TURN OFF READ-DELETED IN CASE SET            
*                                                                               
GREC4    GOTO1 SEQ                             LOOK FOR ANOTHER RECORD          
         LA    R6,KEY                                                           
         OC    TGJWBUNT,TGJWBUNT   JWT JOBS?                                    
         BNZ   GRECX                                                            
         TM    TGAYSTA7,TAAYSBBD   BBDO JOBS?                                   
         BO    GRECX                                                            
         CLC   TLJBKEY(TLJBSEQ-TLJBD),KEYSAVE  WITH ALL SAME UP TO SEQ.         
         BNE   GRECX                                                            
         MVC   AIO,AWRKIO                                                       
         GOTO1 GETREC                                                           
*                                                                               
         L     RE,AIO              NOW COMBINE THEM - RE=A(NEW RECORD)          
         L     R6,AIO1                                R3=A(MAIN RECORD)         
         LH    RF,TLJBLEN-TLJBD(RE)  L'NEW RECORD                               
         SH    RF,DATADISP           LESS DATADISP                              
*                                                                               
         LH    R1,TLJBLEN                                                       
         BCTR  R1,0                                                             
         LR    R0,R1               R0=R1=L'MAIN RECORD (-1 FOR EOR)             
*                                                                               
         AR    R1,RF               PLUS L'NEW RECORD                            
         STH   R1,TLJBLEN          IS L'COMBINED RECORD                         
*                                                                               
         AR    R0,R6               R0=A(END OF MAIN RECORD)                     
         AH    RE,DATADISP         RE=A(1ST EL. IN NEW RECORD)                  
         LR    R1,RF               R1=RF=L'NEW RECORD ELEMENTS                  
*                                                                               
         MVCL  R0,RE               MOVE NEW RECORD AFTER MAIN                   
         B     GREC4               LOOK FOR ANOTHER                             
*                                                                               
GRECX    MVC   KEY,KEYSAVE         RESTORE ORIG. DIR. REC. TO KEY               
         MVC   AIO,AIO1            RESET IOAREA                                 
         B     GRYES                                                            
*                                                                               
GRYES    XR    RC,RC                                                            
GRNO     LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
JOBD     DSECT                                                                  
         DS    0D                                                               
WRKIO    DS    CL4000              WORK IOAREA                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRE3D                                                       
         SPACE 2                                                                
         ORG  SJBWORK                                                           
         DS    0A                                                               
AWRKIO   DS    A                   A(WORKING I/O)                               
ATMPAREA DS    A                   A(TEMPORARY AREA)                            
TMPLNQ   EQU   20000                                                            
*                                                                               
LENGTHS  DS    XL3                                                              
JBS      DS    0CL12                                                            
JBSFRST  DS    CL6                 KEY OF FIRST EL. DISPLAYED ON SCRN           
JBSLAST  DS    CL6                 KEY OF LAST EL DISPLAYED ON SCRN             
STRTJOB  DS    CL6                 FIRST JOB TO DISPLAY                         
ORIGJOB  DS    CL6                 FIRST JOB DIPLAYED ON FIRST PAGE             
THISPG   DS    XL1                 CURRENT PAGE NUMBER                          
NPAGES   DS    XL1                 TOTAL N'PAGES                                
JOBCONT  DS    CL1                                                              
STATUS   DS    XL1                                                              
HAVEREC  EQU   X'80'                                                            
NOJOBS   EQU   X'40'                                                            
JOBKEY   DS    CL32                                                             
SVKEY    DS    CL(L'TLDRREC)       SAVED KEY                                    
*                                                                               
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011TAGENE3   02/26/16'                                      
         END                                                                    
