*          DATA SET PPWIO20    AT LEVEL 015 AS OF 12/17/07                      
*PHASE T41E20A                                                                  
*                                                                               
         TITLE 'T41E20 - FAX MAINT/LIST'                                        
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               CHANGE LOG                                            *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 2                                                                
*                                                                               
*JUL/04        BOBY                BIG BANG                                     
*                                                                               
         TITLE 'T41E20 - FAX MAINT/LIST'                                        
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               T41E20 - DETAIL MAINT/LIST                            *         
*                                                                     *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41E00 (WIO CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, CHG, DISP, SEL, LIST, SEND, DEL         *         
*                                                                     *         
*  INPUTS       SCREEN T41EFC (MAINTENANCE)                           *         
*               SCREEN T41EFA (LIST)                                  *         
*                                                                     *         
*  OUTPUTS                                                            *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- FILED ON SCREEN                                 *         
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
         TITLE 'T41E20 - FAX MAINT/LIST - INIT'                                 
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T41E20   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41E20,RR=RE                                                   
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
         TITLE 'T41E20 - FAX MAINT/LIST - CKMODE'                               
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
*                                                                               
         B     CKMODEX                                                          
*                                                                               
CKMDPFKX DS    0H                                                               
*                                                                               
         CLI   MODE,VALREC         VALREC?                                      
         BNE   CKMDVRN                                                          
*                                                                               
         CLI   ACTNUM,ACTSEND      CHECK IF SEND COMMAND                        
         BNE   CKMDSNDN                                                         
*                                                                               
         BRAS  RE,FILLSCR          FILL SCREEN IF FROM LINKIO                   
*                                                                               
         BRAS  RE,VR               VALIDATE INPUT                               
*                                                                               
         BRAS  RE,SEND             SEND THE FAX                                 
*                                                                               
         B     CKMODEX                                                          
*                                                                               
CKMDSNDN DS    0H                                                               
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
         TITLE 'T41E20 - FAX MAINT/LIST - VK'                                   
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
         CLI   ACTNUM,ACTABDEL     IF ACTION ABDELETE                           
         BNE   *+8                                                              
         BRAS  RE,DK                  DISPLAY THE KEY FIRST                     
*                                                                               
         MVI   CHGSWTCH,0          INITIALIZE CHANGE SWITCH                     
         MVI   DATAFLDS,0          INITIALIZE FIELDS WITH DATA                  
*                                                                               
*        VALIDATE MEDIA                                                         
*                                                                               
         LA    R2,SFXMEDH          POINT TO MEDIA FIELD                         
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         GOTOR VALMED              VALIDATE MEDIA                               
*                                                                               
*        VALIDATE CLIENT                                                        
*                                                                               
         LA    R2,SFXCLTH          POINT TO CLIENT FIELD                        
*                                                                               
         GOTOR VALCLT              VALIDATE CLIENT                              
*                                                                               
*        VALIDATE PUB                                                           
*                                                                               
         LA    R2,SFXPUBH          POINT TO PUB FIELD                           
*                                                                               
         GOTOR VALPUB              VALIDATE PUB                                 
*                                                                               
*        VALIDATE INSORD NUMBER                                                 
*                                                                               
         LA    R2,SFXIO#H          POINT TO INSORD FIELD                        
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
         LA    R2,SFXREV#H         POINT TO REVISION FIELD                      
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
         LA    R2,SFXPERH            PERIOD                                     
*                                                                               
         CLI   FLDILEN,0             PERIOD IS REQUIRED                         
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
*        ALL KEYFIELDS VALIDATED                                                
*                                                                               
*        ANALYZE WHICH FIELDS ENTERED                                           
*                                                                               
         TM    DATAFLDS,DFLIO#Q+DFLRV#Q IF IO# AND REVISION GIVEN               
         BO    VKKEY                       GO BUILD KEY                         
*                                                                               
         CLI   ACTNUM,ACTREP       IF ACTION REPORT                             
         BE    VKKEY                  BUILD ORIGINAL IO KEY                     
*                                                                               
         TM    DATAFLDS,DFLRV#Q    IF RV# WITHOUT IO#                           
         BO    VKRV#ER                ERROR                                     
*                                                                               
         TM    DATAFLDS,DFLFGP#Q   GROUP NUMBER MUST BE MISSING                 
         BO    VKFGP#ER                                                         
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
         LA    R2,SFXIO#H          POINT TO IO# FIELD                           
*                                                                               
         GOTOR DISIO#              DISPLAY IO#                                  
*                                                                               
         LA    R2,SFXREV#H         POINT TO REVISION FIELD                      
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
         GOTOR HIGH                FIND MASTER KEY                              
*                                                                               
         CLC   WIOKEY,KEYSAVE      CHECK IF ON FILE                             
         BNE   VKIO#2ER                                                         
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
         CLI   MINERR,MINESNF      ERROR IF RECORD NOT FOUND                    
         BE    VKIO#2ER                                                         
         CLI   MINERR,0            NO ERRORS TOLERATED                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    MINSTAT,MINDELQ     DONE IF DELETED MINIO SET                    
         BO    VKX                                                              
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
         MVC   SVHDRELM,WIOHDRD    SAVE HEADER ELEMENT                          
*                                                                               
         LA    R2,SFXPERH          POINT TO PERIOD FIELD                        
         MVC   QPER,WIOHSTRT       SAVE PERIOD                                  
*                                                                               
         GOTOR DISPER,DMCB,QPER    DISPLAY PERIOD                               
*                                                                               
*        VALIDATE FAX GROUP NUMBER                                              
*                                                                               
VKFGP#   DS    0H                                                               
*                                                                               
         MVI   QFGP#,0             INIT GROUP NUMBER                            
*                                                                               
         LA    R2,SFXFGP#H         POINT TO GROUP NUMBER FIELD                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        GET INPUT LENGTH                             
         BZ    VKFGP#X             DONE IF NO INPUT                             
*                                                                               
         TM    FLDIIND,FINPNUM     INPUT MUST BE NUMERIC                        
         BNO   VKFGP#ER                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLDDATA(0)      PACK GROUP NUMBER                            
*                                                                               
         CVB   RF,DUB              CVB                                          
*                                                                               
         CHI   RF,256              MUST BE LT 256                               
         BNL   VKFGP#ER                                                         
*                                                                               
         STC   RF,QFGP#            SAVE GROUP NUMBER                            
*                                                                               
         OI    DATAFLDS,DFLFGP#Q   GROUP NUMBER ENTERED                         
*                                                                               
VKFGP#X  DS    0H                                                               
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          BUILD ELEMENT KEY                            
         USING WIOFKEY,R6                                                       
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET FAX ELM CODE                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,QFGP#           GET GROUP NUMBER                            
         BNZ   VKFGP#10               NUMBER SPECIFIED                          
*                                                                               
*                                    NO NUMBER ENTERED                          
*                                                                               
*        FIND LAST GROUP NUMBER IN MINIO SET                                    
*              ACTION DISPLAY WILL SHOW THIS GROUP                              
*              ACTION ADD     WILL SHOW NEXT GROUP NUMBER                       
*                                                                               
         MVC   SVHDRELM,ELEMENT    INIT HEADER SAVEAREA                         
         MVI   SVSTAT,WIOSSNTQ     DEFAULT TO SENT STATUS                       
*                                                                               
         MVI   WIOFKLEN,WIOFKGP#-WIOFKEY  FILTER ON ANY FAX ELEMENT             
*                                                                               
         GOTOR GETELM,DMCB,WIOFKEY  FIND FIRST OF GROUP                         
*                                                                               
VKFLSTLP DS    0H                  FIND LAST GROUP NUMBER                       
*                                                                               
         BNZ   VKFLSTDN            NO MORE GROUPS                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOFKCDE,WIOFKIDQ   DONE IF NOT FAX ELEMENT                      
         BNE   VKFLSTDN                                                         
*                                                                               
         CLI   WIOFKTYP,WIOFKHDQ   SKIP IF NOT HEADER                           
         BNE   VKFLSTCN                                                         
*                                                                               
         MVC   SVHDRELM,0(R6)      SAVE HEADER ELEMENT                          
*                                                                               
         CLI   WIOFSTAT,0          IF GROUP MORE THAN GENERATED                 
         BE    *+8                                                              
         CLI   WIOFSTAT,WIOSGENQ                                                
         BE    *+8                                                              
         MVI   SVSTAT,WIOSRSTQ        THEN THIS SEND MUST BE A RESEND           
*                                                                               
         OC    WIOFGPID,WIOFGPID   IF THERE IS A GROUP ID                       
         BZ    VKFLSTCN                                                         
*                                                                               
         CLC   WIOFGPID,LNKHSTID       USE THIS GROUP IF HISTID MATCH           
         BNE   VKFLSTCN                                                         
*                                                                               
         CLI   WIOFSTAT,0              IF GROUP MORE THAN GENERATED             
         BE    *+8                                                              
         CLI   WIOFSTAT,WIOSGENQ                                                
         BE    *+10                                                             
         MVC   SVSTAT,WIOFSTAT            SAVE GROUP STATUS                     
*                                                                               
         B     VKFLSTDN                                                         
*                                                                               
VKFLSTCN DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOFKEY NEXT ELEMENT                                 
*                                                                               
         B     VKFLSTLP                                                         
*                                                                               
VKFLSTDN DS    0H                                                               
*                                                                               
         LA    R6,SVHDRELM         POINT TO LAST HEADER FOUND                   
*                                                                               
         MVC   QGRPID,WIOFGPID     SET GROUP ID                                 
*                                                                               
         OC    QGRPID,QGRPID       IF NO GROUP ID YET                           
         BNZ   *+10                                                             
         MVC   QGRPID,LNKHSTID         USE ADBUYER'S                            
*                                                                               
         MVI   WRTSW,X'FF'         ASSUME HEADER ALREADY THERE                  
*                                                                               
         OC    WIOFGPID,WIOFGPID   IF THERE IS A GROUP ID                       
         BZ    *+14                                                             
         CLC   WIOFGPID,LNKHSTID      USE THIS GROUP IF HISTID MATCH            
         BE    VKFLSTD1                                                         
*                                                                               
         MVI   WRTSW,0                SET TO WRITE GROUP TO FILE                
*                                                                               
         CLI   ACTNUM,ACTSEND      IF ADBUYER UPLOAD USE NEXT AVAIL #           
         BE    *+8                                                              
         CLI   ACTNUM,ACTADD       IF ADDING, USE NEXT AVAILABLE #              
         BNE   VKFLSTD1                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,WIOFKGP#         GET LAST GROUP NUMBER                        
         AHI   R0,1                   BUMP GROUP NUMBER                         
         STC   R0,WIOFKGP#            SET IN ELEMENT KEY                        
*                                                                               
VKFLSTD1 DS    0H                  DISPLAY LAST GROUP                           
*                                                                               
         MVC   QFGP#,WIOFKGP#      SAVE GROUP NUMBER                            
*                                                                               
VKFLSTX  DS    0H                                                               
*                                                                               
         B     VKFAX30                                                          
*                                                                               
VKFGP#10 DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTDIS       IF ACTION DISPLAY                            
         BE    *+8                                                              
         CLI   ACTNUM,ACTSEL       IF ACTION SELECT                             
         BNE   VKFAX05                CHECK PFKEY                               
*                                                                               
VKFAXPKL DS    0H                                                               
*                                                                               
         CLI   PFAID,8             IF PFKEY 8                                   
         BE    *+8                                                              
         CLI   PFAID,20            OR PFKEY 20 HIT                              
         BNE   *+8                                                              
         AHI   RF,1                   BUMP GROUP NUMBER                         
*                                                                               
         CLI   PFAID,7             IF PFKEY 7                                   
         BE    *+8                                                              
         CLI   PFAID,19            OR PFKEY 19 HIT                              
         BNE   *+8                                                              
         AHI   RF,1                   DECREMENT GROUP NUMBER                    
*                                                                               
         CHI   RF,0                IF TOP OF LIST FOUND                         
         BE    VKFAX2E                NO MORE TO FIND                           
*                                                                               
VKFAX05  DS    0H                                                               
*                                                                               
         STCM  RF,1,WIOFKGP#           SET GROUP NUMBER                         
*                                                                               
         MVI   WIOFKLEN,WIOFKTYP-WIOFKEY SET TO LOOK FOR ANY IN GROUP           
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT READ FOR ELEMENT                             
         BNE   VKFAXNF             ELEMENT NOT FOUND                            
*                                                                               
         CLI   ACTNUM,ACTADD       IF FOUND, ACTION CAN'T BE ADD                
         BE    VKFAX1E                                                          
*                                                                               
         L     RF,MINELEM          POINT TO FOUND ELEMENT                       
         MVC   SVHDRELM,0(RF)      SAVE FOUND DETAIL                            
*                                                                               
         B     VKFAX30                                                          
*                                                                               
VKFAXNF  DS    0H                  ELM NOT FOUND                                
*                                                                               
         CLI   ACTNUM,ACTADD          OKAY IF ACTION ADD                        
         BE    VKFAX30                                                          
*                                                                               
         CLI   ACTNUM,ACTDIS       ERROR IF NOT DISPLAYING                      
         BNE   VKFAX2E                                                          
*                                                                               
         CLI   PFAID,8             ERROR IF NOT PFKEY DOWN                      
         BE    *+8                                                              
         CLI   PFAID,20                                                         
         BE    *+8                                                              
         CLI   PFAID,7             OR PFKEY UP                                  
         BE    *+8                                                              
         CLI   PFAID,19                                                         
         BNE   VKFAX2E                                                          
*                                                                               
         B     VKFAXPKL                                                         
*                                                                               
VKFAX10  DS    0H                  NO FGP# PROVIDED                             
*                                  FIND NEXT AVAILABLE                          
         DC    H'0'                SHOULDN'T GE HERE                            
*                                                                               
         MVI   WIOFKLEN,WIOFKLEN-WIOFKEY  SET FOR COMPARE ON CODE ONLY          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,WIOFKGP#       SAVE CURRENT GROUP NUMBER                    
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT  FIND FIRST DETAIL                           
*                                                                               
VKFAXLP  DS    0H                                                               
*                                                                               
         BNZ   VKFAXDN             END OF DETAILS FOUND                         
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         ICM   R0,1,WIOFKGP#       SAVE FOUND GROUP NUMBER                      
*                                                                               
VKFAXCN  DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,ELEMENT FIND NEXT DETAIL                             
*                                                                               
         B     VKFAXLP                                                          
*                                                                               
VKFAXDN  DS    0H                                                               
*                                                                               
         LA    R6,ELEMENT          RE-POINT TO ELEMENT BUILD AREA               
*                                                                               
         AHI   R0,1                BUMP GROUP NUMBER BY ONE                     
         STCM  R0,1,WIOFKGP#       SET AS NEW GROUP NUMBER                      
*                                                                               
VKFAX30  DS    0H                                                               
*                                                                               
*        RE-DISPLAY FAX GROUP NUMBER                                            
*                                                                               
         EDIT  WIOFKGP#,SFXFGP#,0,ALIGN=LEFT                                    
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         MVI   FLDOLEN,L'SFXFGP#   MAX OUTPUT                                   
*                                                                               
*        GENCON NEEDS A VALUE IN KEY                                            
*                                                                               
         LA    R4,WIOKEY           SET KEY AS MASTER WIO KEY                    
         USING WIOKEY,R4                                                        
*                                                                               
*        GENCON NEEDS A KEY THAT CAN BE FOUND                                   
*                                                                               
         MVC   WIOKELMK,=8X'FF'    SET FOR MASTER MINIO KEY                     
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING                                    
         BNE   *+10                                                             
         XC    WIOKELMK,WIOKELMK      RECORD CAN'T BE FOUND                     
*                                                                               
         MVC   KEY,MINMKEY         SET MASTER PART OF KEY                       
*                                                                               
         CLC   SVIOKEY,QIOKEY     IF INSORD KEY HAS CHANGED                     
         BE    *+16                                                             
         XC    SVHDRELM,SVHDRELM      INIT FAX ELM SAVEAREA                     
         MVC   SVIOKEY,QIOKEY         UPDATE KEY SAVEAREA                       
*                                                                               
         CLI   ACTNUM,ACTABDEL     OR ACTION ABDELETE                           
         BNE   *+8                                                              
         BRAS  RE,DR                  DISPLAY RECORD                            
*                                                                               
*        DETERMINE IF SENDING FAXES AND WHETHER WS SENT DATA FIRST              
*                                                                               
VKSW     DS    0H                                                               
*******                                                                         
*******  MVI   WRTSW,0             INIT WRITE SWITCH                            
*******                                                                         
*******  CLI   ACTNUM,ACTSEND      IF SENDING                                   
*******  BNE   VKSWX                                                            
*******                                                                         
*******  OC    LNKHSTID,LNKHSTID   IF FROM ADBUYER                              
*******  BZ    VKSWX                                                            
*******                                                                         
*******  CLC   QGRPID,LNKHSTID     IF GROUP WAS ALREADY ON FILE                 
*******  BNE   VKSWX                                                            
*******                                                                         
*******  MVI   WRTSW,X'FF'            SET TO NOT WRITE FAX ELMS                 
*******                                                                         
VKSWX    DS    0H                                                               
*                                                                               
VKX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
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
VKFGP#ER LHI   RF,PPEFLDNE        INSORD   REQUIRED                             
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
VKFAX1E  LHI   RF,PPEFAXFD         RECORD ALREADY ON FILE                       
         J     VKERR1                                                           
*                                                                               
VKFAX2E  LHI   RF,PPEFAXNF         INSORD LINE ITEM NOT ON FILE                 
         J     VKERR1                                                           
*                                                                               
VKFAX3E  LHI   RF,PPEFAXNF         INSORD LINE ITEM MUST BE NUMERIC             
         J     VKERR1                                                           
*                                                                               
VKERR    DS    0H                  INSORD RECORD DOES NOT EXIST                 
*                                    CLEAR GROUP # AND PERIOD                   
*                                                                               
         LR    R0,R2               SAVE FIELD POINTER                           
*                                                                               
         LA    R2,SFXFGP#H         POINT TO GROUP NUMBER FIELD                  
         BRAS  RE,CLRFLD           CLEAR INSORD SERIAL NUMBER FIELD             
*                                                                               
         LA    R2,SFXPERH          POINT TO PERIOD FIELD                        
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
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E20 - FAX MAINT/LIST - VKL'                                  
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
         MVI   NLISTS,(LFXLINLH-LFXLIN1H)/(LFXLIN2H-LFXLIN1H)+1                 
*                                                                               
*        VALIDATE MEDIA                                                         
*                                                                               
         LA    R2,LFXMEDH          POINT TO MEDIA FIELD                         
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         GOTOR VALMED              VALIDATE MEDIA                               
*                                                                               
*        VALIDATE CLIENT                                                        
*                                                                               
         LA    R2,LFXCLTH          POINT TO CLIENT FIELD                        
*                                                                               
         GOTOR VALCLT              VALIDATE CLIENT                              
*                                                                               
*        VALIDATE PUB                                                           
*                                                                               
         LA    R2,LFXPUBH          POINT TO PUB FIELD                           
*                                                                               
         GOTOR VALPUB              VALIDATE PUB                                 
*                                                                               
         EJECT                                                                  
*                                                                               
*        VALIDATE INSORD NUMBER                                                 
*                                                                               
         LA    R2,LFXIO#H          POINT TO INSORD FIELD                        
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
         CLC   WIOKEY(WIOKELMK-WIOKEY),KEYSAVE    TEST IF KEY FOUND             
         BNE   VKLIO2ER            MUST FIND KEY                                
*                                                                               
*        READ IN INSORD MASTER RECORD                                           
*                                                                               
         MVC   QIOKEY,WIOKEY       SAVE MASTER KEY                              
         MVC   QIO#,WIOKIO#                                                     
         MVC   QREV#,WIOKRV#                                                    
*                                                                               
         LA    R2,LFXIO#H                                                       
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
         TM    MINSTAT,MINDELQ     SKIP IF DELETED                              
         BO    VKLX                                                             
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
         LA    R2,LFXPERH          POINT TO PERIOD FIELD                        
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
         LA    R2,LFXPERH          POINT TO RUN PERIOD                          
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
         XC    SVHDRELM,SVHDRELM      INIT FAX ELM SAVEAREA                     
         MVC   SVIOKEY,QIOKEY        UPDATE KEY SAVEAREA                        
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT FIELD                           
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
VKLFAX1E LHI   RF,PPEFAXFD         RECORD ALREADY ON FILE                       
         J     VKLERR1                                                          
*                                                                               
VKLFAX2E LHI   RF,PPEFAXNF         INSORD LINE ITEM NOT ON FILE                 
         J     VKLERR1                                                          
*                                                                               
VKLFAX3E LHI   RF,PPEFAXNF         INSORD LINE ITEM MUST BE NUMERIC             
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
         L     R3,0(R1)            SAVE PARAMETER LIST POINTER                  
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
         TITLE 'PPWIO00 - PRINT NEW INSORD CONTROLLER - FIILSCR'                
***********************************************************************         
*                                                                     *         
*        FILLS IN FAX NUMBER FIELDS FROM LINKIO DATA                  *         
*                                                                     *         
*NTRY   LNKAFXNM - A(START OF FAX NUMBER DATA)                        *         
*                                                                     *         
*EXIT    FAX NUMBER FIELDS FILLED IN ON SCREEN                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FILLSCR  NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         ICM   R3,15,LNKAFXNM      POINT TO FIRST FAX NAME DATA ELM             
         BZ    FILLSCRX            NONE AVAILABLE                               
*                                                                               
         USING WKRDATD,R3          ESTABLISH WORKER FILE DATA ELEMENT           
*                                                                               
         LA    R2,SFXNAM1H         POINT TO FIRST FAX NAME ON SCREEN            
         USING FLDHDRD,R2          ESTABLISH STANDARD SCREEN FIELD              
*                                                                               
FSCRLOOP DS    0H                                                               
*                                                                               
         CLI   WKDTRID,0           DONE AT END OF RECORDS                       
         BE    FSCRDONE                                                         
*                                                                               
         CLC   WKDTMPCD,=AL2(D#LINTXT)  DONE AT FIRST TEXT LINE                 
         BE    FSCRDONE                                                         
*                                                                               
         CLC   WKDTMPCD,=AL2(D#RECPNM)  MUST BE FAX NAME                        
         BNE   FSCRCONT                                                         
*                                                                               
*        FAX RECIPIENT NAME                                                     
*                                                                               
FSCRFNM  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WKDTRLEN       GET ELEMENT LENGTH                           
         LR    R0,RF               SAVE ELEMENT LENGTH                          
         SHI   RF,WKDTHDLQ         SUBTRACT HEADER LENGTH                       
         BZ    FSCRCONT              NO DATA                                    
*                                                                               
         STC   RF,FLDILEN          SET INPUT LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),WKDTDATA MOVE TEXT TO SCREEN FIELD                    
*                                                                               
         AR    R3,R0               BUMP TO NEXT DATA ELEMENT                    
*                                                                               
         BRAS  RE,BUMPU            BUMP TO NEXT FIELD ON LINE                   
*                                                                               
FSCRFNMX DS    0H                                                               
*                                                                               
*        POSSIBLY FAX TYPE 1 - INDICATES FAX OR E-MAIL ADDRESS                  
*                                                                               
FSCRTP1  DS    0H                                                               
*                                                                               
         CLC   WKDTMPCD,=AL2(D#TYPE_1)  IGNORE IF NOT FAX/E-MAIL IND            
         BNE   FSCRTP1X                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WKDTRLEN       GET ELEMENT LENGTH                           
         LR    R0,RF               SAVE ELEMENT LENGTH                          
         SHI   RF,WKDTHDLQ         SUBTRACT HEADER LENGTH                       
         BZ    FSCRTP1X              NO DATA                                    
*                                                                               
         AR    R3,R0               BUMP TO NEXT DATA ELEMENT                    
*                                                                               
FSCRTP1X DS    0H                                                               
*                                                                               
*        FAX TYPE 2 - FYI OR PRIMARY INDICATOR                                  
*                                                                               
FSCRTP2  DS    0H                                                               
*                                                                               
         LR    R6,R2               SAVE FAX # FIELD POINTER                     
*                                                                               
         BRAS  RE,BUMPU            BUMP TO NEXT FIELD ON LINE                   
*                                                                               
         CLC   WKDTMPCD,=AL2(D#TYPE_2)  IGNORE IF NOT TYPE-2 DATA               
         BNE   FSCRTP2X                                                         
*                                  ELSE                                         
         SR    RF,RF                                                            
         ICM   RF,3,WKDTRLEN       GET ELEMENT LENGTH                           
         LR    R0,RF               SAVE ELEMENT LENGTH                          
         SHI   RF,WKDTHDLQ         SUBTRACT HEADER LENGTH                       
         BZ    FSCRTP2X              NO DATA                                    
*                                                                               
         STC   RF,FLDILEN          SET INPUT LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),WKDTDATA MOVE TEXT TO SCREEN FIELD                    
*                                                                               
         AR    R3,R0               BUMP TO NEXT DATA ELEMENT                    
*                                                                               
FSCRTP2X DS    0H                                                               
*                                                                               
         BRAS  RE,BUMP             BUMP TO NEXT FIELD - SUPRESS COSTS           
*                                                                               
*        FAX NUMBER                                                             
*                                                                               
FSCRFX#  DS    0H                                                               
*                                                                               
         LR    RF,R2               SAVE A(NEXT FIELD ON LINE)                   
         LR    R2,R6               POINT TO FAX NUMBER FIELD                    
         LR    R6,RF               SAVE A(NEXT FIELD ON LINE)                   
*                                                                               
         CLC   WKDTMPCD,=AL2(D#ADRFAX)  DONE IF NOT FAX NUMBER DATA             
         BNE   FSCRDONE                                                         
*                                  ELSE                                         
         SR    RF,RF                                                            
         ICM   RF,3,WKDTRLEN       GET ELEMENT LENGTH                           
         LR    R0,RF               SAVE ELEMENT LENGTH                          
         SHI   RF,WKDTHDLQ         SUBTRACT HEADER LENGTH                       
         BZ    FSCRFX#X              NO DATA                                    
*                                                                               
         STC   RF,FLDILEN          SET INPUT LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),WKDTDATA MOVE TEXT TO SCREEN FIELD                    
*                                                                               
         AR    R3,R0               BUMP TO NEXT DATA ELEMENT                    
*                                                                               
FSCRFX#X DS    0H                                                               
*                                                                               
*        SUPPRESS COST - Y/N - NOT PRESENT DEFAULTS TO C'N'                     
*                                                                               
FSCRSCS  DS    0H                                                               
*                                                                               
         LR    R2,R6               POINT TO SUPPRESS COST FIELD                 
*                                                                               
         CLC   WKDTMPCD,=AL2(D#SUPCOS) IGNORE IF NOT SUPPRESS COST DATA         
         BNE   FSCRSCSX                                                         
*                                  ELSE                                         
         SR    RF,RF                                                            
         ICM   RF,3,WKDTRLEN       GET ELEMENT LENGTH                           
         LR    R0,RF               SAVE ELEMENT LENGTH                          
         SHI   RF,WKDTHDLQ         SUBTRACT HEADER LENGTH                       
         BZ    FSCRSCSX              NO DATA                                    
*                                                                               
         STC   RF,FLDILEN          SET INPUT LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),WKDTDATA MOVE TEXT TO SCREEN FIELD                    
*                                                                               
         AR    R3,R0               BUMP TO NEXT DATA ELEMENT                    
*                                                                               
FSCRSCSX DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO NEXT FIELD ON LINE (STATUS)          
*                                                                               
         BRAS  RE,BUMPU            BUMP TO NEXT FIELD (NEXT LINE)               
*                                                                               
         CLC   WKDTMPCD,=AL2(D#WIOSTA)  IGNORE IF STATUS FIELD                  
         BNE   FSCRCONT                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WKDTRLEN       GET ELEMENT LENGTH                           
         AR    R3,R0               BUMP TO NEXT ELEMENT                         
*                                                                               
FSCRCONT DS    0H                                                               
*                                                                               
         B     FSCRLOOP                                                         
*                                                                               
FSCRDONE DS    0H                                                               
*                                                                               
FILLSCRX DS    0H                                                               
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
         AHI   RF,-8                  DECREMENT BY EXTENDED SFX LENGTH          
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
         AHI   RF,-8                  DECREMENT BY EXTENDED SFX LENGTH          
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
         TITLE 'PPWIO20 - PRINT NEW FAXS - BUMP'                                
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
         TITLE 'T41E20 - FAX MAINT/LIST - LR'                                   
***********************************************************************         
*                                                                     *         
*        BUILD LIST OF FAXS                                           *         
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
         MVC   ELEMENT,SVFAXELM    LAST FAX ELEMENT READ                        
*                                                                               
         CLI   LRLASTSW,C'Y'       IF END OF LIST LAST TIME                     
         BNE   *+14                                                             
         MVI   LRLASTSW,0             CLEAR SWITCH                              
         XC    ELEMENT,ELEMENT        CLEAR LAST KEY                            
*                                                                               
         LA    R6,ELEMENT                                                       
         USING WIOFAXD,R6          ESTABLISH FAX ELEMENT                        
*                                                                               
         OC    WIOFKEY,WIOFKEY     SKIP IF PRIOR KEY KNOWN                      
         BNZ   LRKEY10                                                          
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET FAX ELEMENT ID                           
         XC    WIOFKGP#,WIOFKGP#   SET FOR FIRST GROUP                          
*                                                                               
LRKEY10  DS    0H                                                               
*                                                                               
         MVI   WIOFKLEN,1          MATCH ON FAX ID                              
*                                                                               
         LA    R4,LISTAR           ESTABLISH LIST LINE                          
         USING LISTLIND,R4                                                      
*                                                                               
         LA    R5,SVLFXSFX         POINT TO KEYS SAVEAREA                       
         XC    SVLFXSFX(SVLFXLNQ),SVLFXSFX  CLEAR TABLE                         
*                                                                               
         GOTO1 GETELM,DMCB,ELEMENT FIND FIRST ELEMENT                           
*                                                                               
LRKEYLP  DS    0H                                                               
*                                                                               
         XC    LISTLIN,LISTLIN     INIT PRINT AREA                              
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOFKCDE,WIOFKIDQ   DONE IF NOT A FAX ELEMENT                    
         BNE   LRKEYDN                                                          
*                                                                               
         CLI   WIOFKTYP,WIOFKHDQ   SKIP IF NOT A HEADER ELEMENT                 
         BNE   LRKEYCN                                                          
*                                                                               
         MVC   SVFAXELM,0(R6)      SAVE FOUND ELEMENT                           
*                                                                               
*        DISPLAY GROUP NUMBER                                                   
*                                                                               
         EDIT  WIOFKGP#,LSFGP#,0,ALIGN=LEFT                                     
*                                                                               
*        DISPLAY GROUP STATUS                                                   
*                                                                               
LRGSTA   DS    0H                                                               
*                                                                               
         SR    R3,R3               INIT POINTER                                 
*                                                                               
         MVC   QSTAT,WIOFSTAT      SET STATUS TO BE DISPLAY                     
         GOTOR DISSTA,DMCB,(L'LSGSTA,LSGSTA)  DISPLAY STATUS                    
*                                                                               
         CLI   WIOFSTAT,WIOSSNTQ   SENT                                         
         BE    *+8                                                              
         CLI   WIOFSTAT,WIOSRSTQ   RE-SENT                                      
         BNE   *+12                                                             
         LA    R3,WIOFSDTE         SENT DATE/TIME                               
         B     LRGSTAX                                                          
*                                  ELSE                                         
         LA    R3,WIOFDDTE         DELIVERED DATE/TIME                          
         B     LRGSTAX                                                          
*                                                                               
LRGSTAX  DS    0H                                                               
*                                                                               
*        DISPLAY GROUP STATUS DATE AND TIME                                     
*                                                                               
         LTR   R3,R3               SKIP IF NO STATUS                            
         BZ    LRGGRPX                                                          
*                                                                               
LRGDTE   DS    0H                                                               
*                                                                               
         GOTOR DATCON,DMCB,(3,0(R3)),(17,LSGDTE) DISPLAY DATE                   
*                                                                               
         LA    R2,FLDH             USE A DUMMY FIELD                            
*                                                                               
         GOTOR DISTIM,DMCB,3(R3)   DISPLAY TIME                                 
*                                                                               
         MVC   LSGTIM,FLD          DISPLAY TIME                                 
*                                                                               
LRGDTEX  DS    0H                                                               
*                                                                               
LRGGRPX  DS    0H                                                               
*                                                                               
*        READ IN SENDER'S E-MAIL ELEMENT                                        
*                                                                               
         GOTOR NXTELM,DMCB,WIOFKEY  READ NEXT ELEMENT                           
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOFKTYP,WIOFKEMQ   LOOKING FOR E-MAIL ELEMENT                   
         BNE   LREMLX                                                           
*                                                                               
*        DISPLAY SENDER'S NAME                                                  
*                                                                               
         MVC   LSENAM,WIOFEML      DISPLAY SENDER'S E-MAIL                      
*                                                                               
LREMLX   DS    0H                                                               
*                                                                               
         MVC   0(L'SVLFXSFX,R5),WIOFKEY  SAVE FAX KEY                           
         LA    R5,L'SVLFXSFX(R5)   BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
         MVC   KEY,MINMKEY         RESET FILE POINTER                           
         GOTOR HIGH                                                             
*                                                                               
         MVC   DMDSKADD,KEY+27                                                  
*                                                                               
         GOTOR LISTMON             PASS BACK TO GENCON                          
*                                                                               
LRKEYCN  DS    0H                                                               
*                                                                               
         GOTO1 NXTELM,DMCB,WIOFKEY    FIND NEXT  ELEMENT                        
*                                                                               
         B     LRKEYLP                                                          
*                                                                               
LRKEYDN  DS    0H                                                               
*                                                                               
         MVI   LRLASTSW,C'Y'       INDICATE END OF DETAILS                      
         XC    SVFAXELM,SVFAXELM   CLEAR WORKAREA                               
*                                                                               
LRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E20 - FAX MAINT/LIST - VR'                                   
***********************************************************************         
*                                                                     *         
*        VALIDATE FAX FIELDS                                          *         
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
*              DELETE FAX NUMBER                                                
*                                                                               
         CLI   ACTNUM,ACTABDEL     IF ACTION ABDELETE                           
         BNE   *+12                                                             
         BRAS  RE,DL                  DELETE LINE ITEM                          
         B     VRX                    ALL DONE                                  
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
*        READ IN HEADER ELEMENT                                                 
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT                                                       
         USING WIOHDRD,R6          ESTABLISH HEADER ELEMENT                     
*                                                                               
         MVI   WIOHKCDE,WIOHKIDQ   SET ELEMENT ID                               
*                                                                               
         GOTOR GETELM,DMCB,WIOHKEY FIND ELEMENT                                 
         BZ    *+6                 MUST FIND ELEMENT                            
         DC    H'0'                                                             
*                                                                               
         TM    MINSTAT,MINDELQ     SKIP IF DELETED                              
         BO    VRX                                                              
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         XC    SVHDRELM,SVHDRELM   INIT SAVEAREA                                
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOHKLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVHDRELM(0),WIOHKEY SAVE HEADER ELEMENT                          
*                                                                               
*        READ IN FAX EMAIL ELEMENT                                              
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT                                                       
         USING WIOFAXD,R6          ESTABLISH FAX ELEMENT                        
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET ELEMENT ID                               
         MVC   WIOFKGP#,QFGP#      SET FAX GROUP NUMBER                         
         MVI   WIOFKTYP,WIOFKEMQ   SET E-MAIL ELEMENT TYPE                      
*                                                                               
         GOTOR GETELM,DMCB,WIOFKEY FIND ELEMENT                                 
         BZ    VREGET10            ELEMENT FOUND                                
*                                                                               
         CLI   ACTNUM,ACTREP       OKAY IF REPORTING (FAXING)                   
         BE    *+8                                                              
         CLI   ACTNUM,ACTSEND      OKAY IF SENDING                              
         BE    *+8                                                              
         CLI   ACTNUM,ACTADD       OKAY IF ADDING                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   WIOFKLEN,WIOFEMLL   SET BASE LENGTH FOR NEW ELM                  
         MVC   SVEMLELM,ELEMENT    INIT E-MAIL ELEMENT                          
*                                                                               
         B     VREGETX                                                          
*                                                                               
VREGET10 DS    0H                                                               
*                                                                               
         L     R1,MINELEM          POINT TO FOUND ELEMENT                       
         SR    RF,RF                                                            
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVEMLELM(0),0(R1)   MOVE ELEMENT TO WORK AREA                    
*                                                                               
VREGETX  DS    0H                                                               
*                                                                               
         TITLE 'T41E20 - FAX MAINT/LIST - VRENAM'                               
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE SENDER'S E-MAIL NAME                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRENAM   DS    0H                                                               
*                                                                               
         LA    R6,SVEMLELM         POINT TO FOUND/NEW ELEMENT                   
*                                                                               
         LA    R2,SFXENMH          POINT TO E-MAIL NAME FIELD                   
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         CLI   FLDILEN,0           OPTIONAL FIELD                               
         BE    VRENAMOK                                                         
*                                                                               
         GOTOR GETFLD              READ IN FIELD (REQUIRED)                     
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        GET INPUT LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WIOFENAM(0),FLDDATA   CHECK CONTACT NAME                         
         BE    VRENAMOK            OKAY IF UNCHANGED                            
*                                                                               
*        NEW E-MAIL NAME                                                        
*                                                                               
         XC    WIOFENAM,WIOFENAM   CLEAR OLD NAME                               
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WIOFENAM(0),FLDDATA SAVE NEW NAME                                
*                                                                               
         OI    SVACH2,WIOAFENM        ENAME CHANGED                             
*                                                                               
VRENAMOK DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
VRENAMX  DS    0H                                                               
*                                                                               
         TITLE 'T41E20 - FAX MAINT/LIST - VREADR'                               
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE E-MAIL ADDRESS                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VREADR   DS    0H                                                               
*                                                                               
         LA    R2,SFXEMLH          POINT TO ADDRESS FLD                         
*                                                                               
         CLI   FLDILEN,0           MUST HAVE AN E-MAIL ADDRESS                  
         BE    VRADRER                                                          
*                                                                               
         GOTOR GETFLD              READ IN FIELD                                
*                                                                               
*        E-MAIL ADDRESS CAN'T START WITH @                                      
*              CAN'T HAVE 2 @'S                                                 
*              CAN'T end in @                                                   
*                                                                               
         CLI   FLDDATA,C'@'        CAN'T BE A DOMAIN NAME                       
         BE    VREADRE1                                                         
*                                                                               
         LA    R0,C'@'             SET SEARCH CHARACTER                         
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,FLDILEN        INPUT LENGTH                                 
*                                                                               
         LA    RE,FLDDATA(RE)      END OF INPUT                                 
         LR    R1,RE               SAVE END ADDRESS                             
*                                                                               
         LA    RF,FLDDATA+1        BYPASS FIRST POSITION                        
*                                                                               
         SRST  RE,RF               SEARCH FOR @ SIGN                            
         BNL   VREADRE4              NOT FOUND - ERROR                          
*                                                                               
         LA    RF,1(RE)            START OF NEXT SEARCH                         
         LR    RE,R1               RESET END OF SEARCH                          
*                                  FOUND                                        
         CR    RE,RF               CAN'T END IN @                               
         BE    VREADRE3                                                         
*                                                                               
         SRST  RE,RF               LOOK FOR SECOND @ SIGN                       
         BL    VREADRE2            SECOND @ FOUND                               
*                                                                               
*        HAVE FOUND VALID ADDRESS                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDILEN          INPUT LENGTH                                 
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WIOFEML(0),FLDDATA    CHECK FOR NEW ADDRESS                      
         BE    VREADROK            OKAY IF UNCHANGED                            
*                                                                               
*        NEW E-MAIL ADDRESS                                                     
*                                                                               
         SR    RE,RE                                                            
         IC    RE,WIOFKLEN         GET ELEMENT LENGTH                           
         SHI   RE,WIOFEMLL         DECREMENT BY BASE ELM LENGTH                 
         BNP   *+6                 NO PRIOR EMAIL ADDRESS                       
         BCTR  RE,0                DECREMENT FOR EXECUTE                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    WIOFEML(0),WIOFEML  CLEAR OLD ADDRESS                            
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WIOFEML(0),FLDDATA    SAVE E-MAIL ADDRESS                        
*                                                                               
         AHI   RF,WIOFEMLL+1       CALCULATE ELEMENT LENGTH                     
         STC   RF,WIOFKLEN         SET NEW ELEMENT LENGTH                       
*                                                                               
         TM    FLDIIND,FINPTHIS    IF FIELD INPUT THIS TIME                     
         BNO   *+8                                                              
         OI    SVACH2,WIOAFEAD        E-MAIL ADDRESS CHANGED                    
*                                                                               
VREADROK DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
VREADRX  DS    0H                                                               
*                                                                               
         TITLE 'T41E20 - FAX MAINT/LIST - VRFAX'                                
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE FAX NUMBERS                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRFAX    DS    0H                                                               
*                                                                               
*        COLLECT FAX NUMBERS ON FILE IN A TABLE                                 
*                                                                               
         L     R3,=A(SVFAXTB)                                                   
         A     R3,RELO20           RELOCALTE ADDRESS                            
*                                                                               
         XC    0(SVFAXTBL,R3),0(R3) INIT FIRST ENTRY                            
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT                                                       
         USING WIOFAXD,R6          ESTABLISH FAX ELEMENT                        
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET ELEMENT CODE                             
         MVI   WIOFKLEN,WIOFKSQN-WIOFKEY  MATCH ON GROUP NUMBER                 
         MVI   WIOFKTYP,WIOFKFXQ   SET AS FAX   ELEMENT                         
         MVC   WIOFKGP#,QFGP#      SET GROUP NUMBER                             
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT FIND FIRST ELEMENT                           
*                                                                               
         MVI   WGRPSW,C'N'         ASSUME A NEW GROUP                           
*                                                                               
VRTABLP  DS    0H                                                               
*                                                                               
         BNZ   VRTABDN             NO MORE ELEMENTS                             
*                                                                               
         MVI   WGRPSW,0            GROUP IS AN OLD ONE                          
*                                                                               
         CLI   WIOFKFTP,WIOFKFAQ   SKIP IF NOT FAX ADDR ELM                     
         BNE   VRTABCN                                                          
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
         MVC   0(SVFAXTBL,R3),WIOFKEY  SAVE IN TABLE                            
*                                                                               
         LA    R3,SVFAXTBL(R3)     BUMP TO NEXT ENTRY IN THE TABLE              
         XC    0(SVFAXTBL,R3),0(R3) INIT NEXT ENTRY                             
*                                                                               
VRTABCN  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOFKSQN         BUMP SEQUENCE #                              
         AHI   RF,1                                                             
*                                                                               
         MVI   WIOFKFTP,0                                                       
*                                                                               
         LA    R6,ELEMENT                                                       
         STC   RF,WIOFKSQN                                                      
*                                                                               
         MVI   WIOFKLEN,WIOFKSQN-WIOFKEY  MATCH ON GROUP NUMBER                 
*                                                                               
         GOTOR GETELM,DMCB,WIOFKEY FIND NEXT ELEMENT                            
*                                                                               
         B     VRTABLP                                                          
*                                                                               
*        FILTER ON KEY                                                          
*                                                                               
         CLI   WIOFKCDE,WIOFKIDQ   TEST FAX ELM CODE                            
         BNE   *+10                                                             
         CLC   WIOFKGP#,QFGP#      TEST GROUP NUMBER                            
         BNE   *+8                                                              
         CLI   WIOFKTYP,WIOFKFXQ   TEST FAX NUMBER ELEMENT                      
         BNE   *+8                                                              
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
VRTABDN  DS    0H                                                               
*                                                                               
*        VALIDATE ENTRIES ON SCREEN                                             
*                                                                               
*        BUILD SKELETON ELEMENT                                                 
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT                                                       
         USING WIOFAXD,R6          ESTABLISH FAX ELEMENT                        
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET ELEMENT CODE                             
         MVI   WIOFKLEN,WIOFFAXL   SET BASIC ELEMENT LENGTH                     
         MVI   WIOFKTYP,WIOFKFXQ   SET AS FAX   ELEMENT                         
         MVC   WIOFKGP#,QFGP#      SET GROUP NUMBER                             
         MVI   WIOFKSQN,1          INIT SEQUENCE NUMBER                         
*                                                                               
         LA    R0,((SFXNAMLH-SFXNAM1H)/(SFXNAM2H-SFXNAM1H))+1  FAXS             
*                                                                               
         LA    R2,SFXNAM1H         POINT TO FIRST NAME FIELD                    
*                                                                               
VRFAXLP  DS    0H                                                               
*                                                                               
         LA    R6,ELEMENT                                                       
         USING WIOFAXD,R6          ESTABLISH FAX ELEMENT                        
*                                                                               
         TITLE 'T41E20 - FAX MAINT/LIST - VRFNAM'                               
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE FAX RECIPIENT'S NAME                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRFNAM   DS    0H                                                               
*                                                                               
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         XC    WIOFXNAM,WIOFXNAM   INIT RECIPIENT'S NAME                        
*                                                                               
         CLI   FLDILEN,0           DONE IF NO NAME                              
         BE    VRFAXDN                                                          
*                                                                               
         GOTOR GETFLD              READ IN FIELD (REQUIRED)                     
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        GET INPUT LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WIOFXNAM(0),FLDDATA   SAVE CONTACT NAME                          
*                                                                               
VRFNAMOK DS    0H                                                               
*                                                                               
*        FIND ENTRY IN TABLE OF FAXES                                           
*                                                                               
         L     R3,=A(SVFAXTB)                                                   
         A     R3,RELO20           RELOCATE ADDRESS                             
*                                                                               
VRFFTBLP DS    0H                                                               
*                                                                               
         OC    0(SVFAXTBL,R3),0(R3) DONE AT END OF TABLE                        
         BZ    VRFFTBDN                                                         
*                                                                               
         CLI   WGRPSW,C'N'         SKIP IF NEW GROUP                            
         BE    VRFFTBCN               ALLOWS DUPLICATE ENTRIES                  
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WIOFXNAM(0),WIOFXNAM-WIOFKEY(R3)  MATCH ON CONTACT NAME          
         BE    VRFFTBFD                                                         
*                                                                               
VRFFTBCN DS    0H                                                               
*                                                                               
         LA    R3,SVFAXTBL(R3)     BUMP TO NEXT TABLE ENTRY                     
         B     VRFFTBLP                                                         
*                                                                               
VRFFTBFD DS    0H                                                               
*                                                                               
         LR    R6,R3               USE TABLE ENTRY AS OLD COPY                  
*                                                                               
         B     VRFFTBX                                                          
*                                                                               
VRFFTBDN DS    0H                                                               
*                                                                               
         MVC   0(SVFAXTBL,R3),WIOFKEY ADD NEW ENTRY TO TABLE                    
*                                                                               
         LR    R6,R3               USE THIS TABLE ENTRY                         
*                                                                               
         LA    R3,SVFAXTBL(R3)     BUMP TO NEXT TABLE ENTRY                     
         XC    0(SVFAXTBL,R3),0(R3) INIT NEXT ELEMENT                           
*                                                                               
         OI    SVACH2,WIOAFXNM     RECIPIENT'S NAME CHANGED                     
*                                                                               
VRFFTBX  DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
VRFNAMX  DS    0H                                                               
*                                                                               
         TITLE 'PPWIO20 - VALIDATE FAX NUMBER - VRFX#'                          
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE FAX NUMBER - MUST BE NUMERIC             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRFX#    DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO FAX NUMBER                           
*                                                                               
         CLI   FLDILEN,0           MUST HAVE A FAX NUMBER                       
         BE    VRFX#ER                                                          
*                                                                               
         GOTOR GETFLD              READ IN FIELD                                
*                                                                               
*        EXTRACT ALL NUMERIC DIGITS FROM INPUT                                  
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,FLDILEN        GET LENGTH OF INPUT                          
         XC    WORK,WORK           INIT WORKAREA                                
         LA    RF,FLDDATA          POINT TO INPUT                               
         LA    RE,WORK                                                          
         SR    R3,R3               INIT 1ST # FLAG                              
*                                                                               
VRFX#LP  DS    0H                                                               
*                                                                               
         CLI   0(RF),C'0'          DROP NON-NUMERIC CHARACTERS                  
         BL    VRFX#CN                                                          
         CLI   0(RF),C'9'                                                       
         BH    VRFX#CN                                                          
*                                                                               
         LTR   R3,R3               IF FIRST DIGIT                               
         BNZ   *+16                                                             
         LA    R3,1                SET 1ST DIGIT FLAG OFF                       
         CLI   0(RF),C'1'          AND IT IS 1                                  
         BE    VRFX#CN                DROP                                      
*                                                                               
         MVC   0(1,RE),0(RF)       SAVE NUMERIC DIGIT                           
         AHI   RE,1                BUMP TO NEXT SAVE POSITION                   
*                                                                               
VRFX#CN  DS    0H                                                               
*                                                                               
         AHI   RF,1                BUMP TO NEXT INPUT CHARACTER                 
         BCT   R1,VRFX#LP                                                       
*                                                                               
VRFX#DN  DS    0H                                                               
*                                                                               
         LA    RF,WORK             CALCULATE LENGTH OF FAX NUMBER               
         SR    RE,RF                                                            
         STC   RE,HALF             SAVE INPUT FAX # LENGTH                      
*                                                                               
*        EXTRACT ALL NUMERIC DIGITS FROM FILE FAX #                             
*                                                                               
         LA    R1,L'WIOFX#         GET LENGTH OF FILE FAX #                     
         LA    RF,WIOFX#           POINT TO INPUT                               
         LA    RE,WORK+32                                                       
         SR    R3,R3               INIT 1ST DIGIT FLAG                          
*                                                                               
VRFX#1LP DS    0H                                                               
*                                                                               
         CLI   0(RF),C'0'          DROP NON-NUMERIC CHARACTERS                  
         BL    VRFX#1CN                                                         
         CLI   0(RF),C'9'                                                       
         BH    VRFX#1CN                                                         
*                                                                               
         LTR   R3,R3               IF FIRST DIGIT                               
         BNZ   *+16                                                             
         LA    R3,1                SET 1ST DIGIT FLAG OFF                       
         CLI   0(RF),C'1'          AND IT IS 1                                  
         BE    VRFX#1CN                DROP                                     
*                                                                               
         MVC   0(1,RE),0(RF)       SAVE NUMERIC DIGIT                           
         AHI   RE,1                BUMP TO NEXT SAVE POSITION                   
*                                                                               
VRFX#1CN DS    0H                                                               
*                                                                               
         AHI   RF,1                BUMP TO NEXT INPUT CHARACTER                 
         BCT   R1,VRFX#1LP                                                      
*                                                                               
VRFX#1DN DS    0H                                                               
*                                                                               
         LA    RF,WORK+32          CALCULATE LENGTH OF FAX NUMBER               
         SR    RE,RF                                                            
         BNP   VRFX#10             OKAY IF THERE WAS NO PRIOR #                 
*                                                                               
         CLM   RE,1,HALF           ERROR IF # OF DIGITS DIFFERS                 
         BNE   VRFX#1E                                                          
*                                                                               
         LR    RF,RE               FAX NUMBER LENGTH                            
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WORK+32(0),WORK     CHECK FAX NUMBER                             
         BE    VRFX#OK             OKAY IF UNCHANGED                            
*                                                                               
         OC    WIOFXFDT,WIOFXFDT   IF E-MAIL ALREADY SENT                       
         BNZ   VRFX#1E               CAN'T CHANGE FAX NUMBER                    
*                                                                               
*        NEW FAX NUMBER                                                         
*                                                                               
VRFX#10  DS    0H                                                               
*                                                                               
         XC    WIOFX#,WIOFX#       CLEAR OLD NUMBER                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,HALF             GET INPUT LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WIOFX#(0),WORK      SAVE FAX NUMBER                              
*                                                                               
         OI    SVACH2,WIOAFX#      FAX NUMBER CHANGED                           
*                                                                               
VRFX#OK  DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
VRFX#X   DS    0H                                                               
*                                                                               
         TITLE 'PPWIO20 - VALIDATE FAX TYPE - VRFTYP'                           
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE FAX TYPE - FYI                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRFTYP   DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO FAX NUMBER                           
*                                                                               
         CLI   FLDILEN,0           OKAY IF FIELD NOT ENTERED                    
         BE    VRFTYPOK                                                         
*                                                                               
         GOTOR GETFLD              READ IN FIELD                                
*                                                                               
         CLI   FLDDATA,C'F'        FAX COULD BE FYI                             
         BE    *+8                                                              
         CLI   FLDDATA,C'P'        FAX COULD BE PRIMARY                         
         BNE   VRTYPE1                                                          
*                                                                               
         MVC   WORK(1),FLDDATA     COPY TYPE                                    
*                                                                               
         CLI   WORK,C'P'           IF PRIMARY TYPE                              
         BNE   *+8                                                              
         MVI   WORK,0                 FORCE TO NULLS                            
*                                                                               
         CLC   WIOFXTYP,WORK       SKIP IF TYPE UNCHANGED                       
         BE    VRFTYPOK                                                         
*                                                                               
         MVC   WIOFXTYP,WORK       SAVE NEW TYPE                                
*                                                                               
VRFTYPOK DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
VRFTYPX  DS    0H                                                               
*                                                                               
         TITLE 'PPWIO20 - VALIDATE FAX TYPE - VRSCS'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE SUPPRESS COSTS - Y/N                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRSCS    DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO SUPPRESS COSTS FIELD                 
*                                                                               
         CLI   FLDILEN,0           OKAY IF FIELD NOT ENTERED                    
         BE    VRSCSOK                DEFAULTS TO NO                            
*                                                                               
         GOTOR GETFLD              READ IN FIELD                                
*                                                                               
         CLI   FLDDATA,C'Y'        FAX COULD BE C'Y' - SUPPRESS                 
         BE    *+8                                                              
         CLI   FLDDATA,C'N'        FAX COULD BE C'N' - DON'T SUPPRESS           
         BNE   VRSCSE1                                                          
*                                                                               
         MVC   WORK(1),FLDDATA     COPY TYPE                                    
*                                                                               
         CLI   WORK,C'N'           IF DON'T SUPPRESS                            
         BNE   *+8                                                              
         MVI   WORK,0                 FORCE TO NULLS                            
*                                                                               
         CLC   WIOFXSCS,WORK       SKIP IF SUPPRESS UNCHANGED                   
         BE    VRSCSOK                                                          
*                                                                               
         MVC   WIOFXSCS,WORK       SAVE NEW TYPE                                
*                                                                               
VRSCSOK DS     0H                                                               
*                                                                               
         MVC   SVSCS,WIOFXSCS      SAVE SUPPRESS OPTION                         
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
VRSCSX   DS    0H                                                               
*                                                                               
         TITLE 'PPWIO20 - VALIDATE FAX STATUS - VRFSTA'                         
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE FAX STATUS                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRFSTA   DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO FAX NUMBER                           
*                                                                               
         CLI   FLDILEN,0           OKAY IF FIELD NOT ENTERED                    
         BE    VRFSTAOK                                                         
*                                                                               
         GOTOR VALSTA              VALIDATE STATUS                              
*                                                                               
         BE    VRFSTAOK                                                         
*                                                                               
         MVC   WIOFXSTA,QSTAT      SAVE NEW STATUS                              
*                                                                               
         OI    SVACH2,WIOAFXST     STATUS CHANGED                               
*                                                                               
VRFSTAOK DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
VRFSTAX  DS    0H                                                               
*                                                                               
         TITLE 'PPWIO20 - VALIDATE FAX NUMBER - VRFAXCN'                        
***********************************************************************         
*                                                                     *         
*        CONTINUATION OF FAX PROCESSING LOOP                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRFAXCN  DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            NEXT UNPROTECTED FIELD                       
*                                                                               
         BCT   R0,VRFAXLP          PROCESS NEXT LINE                            
*                                                                               
VRFAXDN  DS    0H                                                               
*                                                                               
*        ADD HEADER ELEMENT                                                     
*                                                                               
         CLI   WRTSW,X'FF'         SKIP IF WEBSERVER ALREADY SENT DATA          
         BE    VRFAXX                                                           
*                                                                               
         XC    ELEMENT,ELEMENT     INIT GROUP HEADER ELEMENT                    
         LA    R6,ELEMENT          ESTABLISH GROUP HEADER ELEMENT               
         USING WIOFAXD,R6                                                       
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET AS FAX ELEMENT                           
         MVI   WIOFKLEN,WIOFHDRL   SET LENGTH OF GROUP HEADER                   
         MVC   WIOFKGP#,QFGP#      SET GROUP NUMBER                             
         MVI   WIOFKTYP,WIOFKHDQ   SET AS GROUP HEADER                          
*                                                                               
         MVC   WIOFGPID,LNKHSTID   SET GROUP ID                                 
*                                                                               
         L     R3,WRTELM           ASSUME RE-WRITING ELEMENT                    
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT  READ HEADER ELEMENT                         
         BZ    *+8                 FOUND                                        
         L     R3,ADDELM              SWITCH TO ADDING ELEMENT                  
*                                                                               
         GOTOR (R3),DMCB,ELEMENT    ELEMENT TO RECORD                           
         BZ    *+6                 MUST SUCEED                                  
         DC    H'0'                                                             
*                                                                               
*        ADD E-MAIL ELEMENT                                                     
*                                                                               
         L     R3,WRTELM           ASSUME RE-WRITING ELEMENT                    
*                                                                               
         GOTOR GETELM,DMCB,SVEMLELM  READ E-MAIL ELEMENT                        
         BZ    *+8                 FOUND                                        
         L     R3,ADDELM              SWITCH TO ADDING ELEMENT                  
*                                                                               
         GOTOR (R3),DMCB,SVEMLELM   ELEMENT TO RECORD                           
         BZ    *+6                 MUST SUCCEED                                 
         DC    H'0'                                                             
*                                                                               
VRFAXX   DS    0H                                                               
*                                                                               
*        DELETE FAX ELEMENTS                                                    
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT                                                       
         USING WIOFAXD,R6          ESTABLISH FAX ELEMENT                        
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET ELEMENT CODE                             
         MVI   WIOFKLEN,WIOFKSQN-WIOFKEY  SET BASIC ELEMENT LENGTH              
         MVI   WIOFKTYP,WIOFKFXQ   SET AS FAX ELEMENT                           
         MVC   WIOFKGP#,QFGP#      SET GROUP NUMBER                             
*                                                                               
         MVC   SVKEY,ELEMENT       SAVE SKELETON KEY                            
*                                                                               
VRFDELLP DS    0H                                                               
*                                                                               
         GOTOR GETELM,DMCB,WIOFKEY FIND NEXT FAX ELEMENT                        
         BNZ   VRFDELDN            NO MORE ELEMENTS                             
*                                                                               
         ICM   R1,15,MINELEM       POINT TO FOUND ELEMENT                       
         MVC   ELEMENT,0(R1)                                                    
*                                                                               
         GOTOR DELELM,DMCB,WIOFKEY DELETE ELEMENT                               
*                                                                               
VRFDELCN DS    0H                                                               
*                                                                               
         LA    R6,ELEMENT          RE-POINT TO KEY AREA                         
         MVC   WIOFKEY,SVKEY       RESTORE SKELETON KEY                         
*                                                                               
         B     VRFDELLP                                                         
*                                                                               
VRFDELDN DS    0H                                                               
*                                                                               
*        ADD FAX ELEMENTS                                                       
*                                                                               
VRFADD   DS    0H                                                               
*                                                                               
****     LA    R6,SVFAXTB          POINT TO FAX ELEMENTS TABLE                  
         L     R6,=A(SVFAXTB)                                                   
         A     R6,RELO20           RELOCALTE ADDRESS                            
*                                                                               
         SR    R4,R4               INIT SEQUENCE NUMBER                         
*                                                                               
         L     RF,ADDELM           WILL BE ADDING ELEMENTS                      
*                                                                               
VRFADDLP DS    0H                                                               
*                                                                               
         OC    WIOFXNAM,WIOFXNAM   SKIP IF NO FAX NAME PRESENT                  
         BZ    VRFADDCN                                                         
*                                                                               
         AHI   R4,1                BUMP SEQUENCE NUMBER                         
*                                                                               
         STC   R4,WIOFKSQN         SET NEW SEQUENCE NUMBER                      
*                                                                               
         XC    ELEMENT,ELEMENT     MOVE TO WORKAREA                             
         MVC   ELEMENT(SVFAXTBL),WIOFKEY                                        
*                                                                               
         GOTOR (RF),DMCB,ELEMENT   ADD ELEMENT TO MINIO SET                     
*                                                                               
VRFADDCN DS    0H                                                               
*                                                                               
         LA    R6,SVFAXTBL(R6)     BUMP TO NEXT ELM IN TABLE                    
*                                                                               
         BCT   R0,VRFADDLP         LOOP THROUGH TABLE                           
*                                                                               
VRFADDDN DS    0H                                                               
*                                                                               
*        UPDATE ACTIVITY ELEMENT                                                
*                                                                               
         OI    SVACH1,WIOAFADD     FAX ADDED                                    
*                                                                               
         GOTOR ACTPUT              UPDATE ACTIVITY                              
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
         CLI   MINERR,0            MUST SUCCEED                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,DR               DISPLAY FAX GROUP                            
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
VRENME1  LHI   RF,PPENOCHG         CAN'T CHANGE BECAUSE E-MAIL SENT             
         J     VRERR                                                            
*                                                                               
VRADRER  LHI   RF,PPEADRMS         E-MAIL ADDRESS REQUIRED                      
         J     VRERR                                                            
*                                                                               
VREADRE1 LHI   RF,PPENOCHG         CAN'T CHANGE BECAUSE E-MAIL SENT             
         J     VRERR                                                            
*                                                                               
VREADRE2 LHI   RF,PPEDTENV         TOO MANY @'S IN E-MAIL ADDRESS               
         J     VRERR                                                            
*                                                                               
VREADRE3 LHI   RF,PPEDTENV         E-MAIL ADDRESS CAN'T END IN @                
         J     VRERR                                                            
*                                                                               
VREADRE4 LHI   RF,PPEDTENV         NO @ IN E-MAIL ADDRESS                       
         J     VRERR                                                            
*                                                                               
VRFNME1  LHI   RF,PPENOCHG         CAN'T CHANGE BECAUSE E-MAIL SENT             
         J     VRERR                                                            
*                                                                               
VRFX#ER  LHI   RF,PPEFX#MS         FAX NUMBER REQUIRED                          
         J     VRERR                                                            
*                                                                               
VRFX#1E  LHI   RF,PPENOCHG         CAN'T CHANGE BECAUSE E-MAIL SENT             
         J     VRERR                                                            
*                                                                               
VRTYPE1  LHI   RF,PPEDTENV         STATUS NOT VALID                             
         J     VRERR                                                            
*                                                                               
VRSCSE1  LHI   RF,PPEFLDNV         SUPPRESS COSTS OPTION NOT VALID              
         J     VRERR                                                            
*                                                                               
VRSTAE1  LHI   RF,PPEDTENV         STATUS NOT VALID                             
         J     VRERR                                                            
*                                                                               
VRFAXE2  LHI   RF,PPEDTENV         CAN'T DELETE BECAUSE E-MAIL SENT             
         J     VRERR                                                            
*                                                                               
VRERR    DS    0H                                                               
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
*                                                                               
         GOTOR ERREXIT                                                          
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
         TITLE 'T41E20 - FAX MAINT/LIST - DR'                                   
***********************************************************************         
*                                                                     *         
*        DISPLAY  FAX FIELDS                                          *         
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
         TM    MINSTAT,MINDELQ     SKIP IF DELETED                              
         BO    DRX                                                              
*                                                                               
*        READ IN GROUP HEADER ELEMENT                                           
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT          ESTABLISH DETAIL ELEMENT KEY                 
         USING WIOFAXD,R6          ESTABLISH FAX ELEMENT                        
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET ELEMENT ID                               
         MVC   WIOFKGP#,QFGP#      SET FAX GROUP NUMBER                         
         MVI   WIOFKTYP,WIOFKHDQ   SET HEADER ELEMENT TYPE                      
*                                                                               
         GOTOR GETELM,DMCB,WIOFKEY FIND ELEMENT                                 
         BNZ   DRX                 NO FAX DATA                                  
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
*                                                                               
         L     R1,MINELEM          POINT TO FOUND ELEMENT                       
         SR    RF,RF                                                            
         IC    RF,1(R1)            GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WIOFAXD(0),0(R1)    MOVE ELEMENT TO WORK AREA                    
*                                                                               
         TITLE 'T41E20 - FAX MAINT/LIST - DRGSTA'                               
***********************************************************************         
*                                                                     *         
*        DISPLAY  DISPLAY GROUP STATUS, DATE AND TIME                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRGSTA   DS    0H                                                               
*                                                                               
         LA    R2,SFXGSTAH         POINT TO GROUP STATUS FIELD                  
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD HEADER                
*                                                                               
         BRAS  RE,CLRFLD           CLEAR THE FIELD                              
*                                                                               
         MVC   QSTAT,WIOFSTAT      SET STATUS TO BE DISPLAYED                   
*                                                                               
         GOTOR DISSTA              DISPLAY THE STATUS                           
*                                                                               
         CLI   WIOFSTAT,WIOSSNTQ   SENT                                         
         BE    *+8                                                              
         CLI   WIOFSTAT,WIOSSNTQ   RE-SENT                                      
         BNE   *+12                                                             
         LA    R3,WIOFSDTE         SENT DATE/TIME                               
         B     DRGSTAX                                                          
*                                  ELSE                                         
         LA    R3,WIOFDDTE         DELIVERED DATE/TIME                          
*                                                                               
DRGSTAX  DS    0H                                                               
*                                                                               
*        DISPLAY GROUP STATUS DATE                                              
*                                                                               
DRGDT    DS    0H                                                               
*                                                                               
         LA    R2,SFXGDTH          POINT TO STATUS TIME                         
*                                                                               
         BRAS  RE,CLRFLD           CLEAR THE FIELD                              
*                                                                               
         OC    0(3,R3),0(R3)       SKIP IF NO DATE GIVEN                        
         BZ    DRGDTX                                                           
*                                                                               
         GOTOR DATCON,DMCB,(3,0(R3)),(17,FLDDATA) DISPLAY DATE                  
*                                                                               
DRGDTX   DS    0H                                                               
*                                                                               
*        DISPLAY GROUP STATUS TIME                                              
*                                                                               
DRGTM    DS    0H                                                               
*                                                                               
         LA    R2,SFXGTMH          POINT TO STATUS TIME                         
*                                                                               
         BRAS  RE,CLRFLD           CLEAR THE FIELD                              
*                                                                               
         OC    3(3,R3),3(R3)       SKIP IF NO TIME ENTERED                      
         BZ    DRGTMX                                                           
*                                                                               
         GOTOR DISTIM,DMCB,(3,3(R3))   DISPLAY TIME                             
*                                                                               
DRGTMX   DS    0H                                                               
*                                                                               
         TITLE 'T41E20 - FAX MAINT/LIST - DREMAIL'                              
***********************************************************************         
*                                                                     *         
*        DISPLAY  SENDER'S EMAIL ADDRESS AND STATUS                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DREMAIL  DS    0H                                                               
*                                                                               
*        READ IN E-MAIL ELEMENT                                                 
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          ESTABLISH E-MAIL ELM KEY                     
         USING WIOFKEY,R6                                                       
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET ELEMENT ID                               
         MVC   WIOFKGP#,QFGP#      SET FAX GROUP NUMBER                         
         MVI   WIOFKTYP,WIOFKEMQ   SET E-MAIL ELEMENT TYPE                      
*                                                                               
         GOTO1 GETELM,DMCB,WIOFKEY FIND ELEMENT                                 
         BNZ   DREMAILX            ELEMENT NOT FOUND                            
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
*                                                                               
         L     R1,MINELEM          POINT TO FOUND ELEMENT                       
         SR    RF,RF                                                            
         IC    RF,1(R1)            GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WIOFAXD(0),0(R1)    MOVE ELEMENT TO WORK AREA                    
*                                                                               
*        DISPLAY SENDER'S NAME                                                  
*                                                                               
DRENAM   DS    0H                                                               
*                                                                               
         LA    R2,SFXENMH          POINT TO NAME FIELD                          
*                                                                               
         BRAS  RE,CLRFLD           CLEAR THE FIELD                              
*                                                                               
         MVC   FLDDATA(L'SFXENM),WIOFENAM DISPLAY SENDER'S NAME                 
*                                                                               
         BRAS  RE,SETLEN           FILL IN TRUE LENGTH OF DATA                  
*                                                                               
DRENAMX  DS    0H                                                               
*                                                                               
*        DISPLAY E-MAIL STATUS                                                  
*                                                                               
         LA    R2,SFXESTAH         POINT TO E-MAIL STATUS                       
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD HEADER                
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
*        DISPLAY E-MAIL ADDRESS                                                 
*                                                                               
DREML    DS    0H                                                               
*                                                                               
         LA    R2,SFXEMLH          POINT TO SENDER'S E-MAIL ADDR                
*                                                                               
         BRAS  RE,CLRFLD           CLEAR THE FIELD                              
*                                                                               
         MVC   FLDDATA(L'SFXEML),WIOFEML  DISPLAY SENDER'S ADDR                 
*                                                                               
         BRAS  RE,SETLEN           FILL IN TRUE LENGTH OF DATA                  
*                                                                               
DREMAILX DS    0H                                                               
*                                                                               
         TITLE 'T41E20 - FAX MAINT/LIST - DRFAX'                                
***********************************************************************         
*                                                                     *         
*        ROUTINE TO DISPLAY  FAX NUMBERS                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRFAX    DS    0H                                                               
*                                                                               
*        READ FAX ELEMENTS                                                      
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT                                                       
         USING WIOFAXD,R6          ESTABLISH FAX ELEMENT                        
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET ELEMENT CODE                             
         MVI   WIOFKLEN,WIOFFAXL   SET BASIC ELEMENT LENGTH                     
         MVI   WIOFKTYP,WIOFKFXQ   SET AS FAX NUMBER ELEMENT                    
         MVC   WIOFKGP#,QFGP#      SET GROUP NUMBER                             
         MVI   WIOFKSQN,1          INIT SEQUENCE NUMBER                         
*                                  NUMBER OF FAX ENTRIES ON SCREEN              
         LA    R0,((SFXNAMLH-SFXNAM1H)/(SFXNAM2H-SFXNAM1H))+1  FAXS             
*                                                                               
         LA    R2,SFXNAM1H         POINT TO FIRST NAME FIELD                    
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         MVI   WIOFKLEN,WIOFKSQN-WIOFKEY  SET FILTER LENGTH                     
*                                                                               
         GOTOR GETELM,DMCB,WIOFKEY FIND ELEMENT                                 
*                                                                               
DRFAXLP  DS    0H                                                               
*                                                                               
         BNZ   *+8                 ELEMENT NOT FOUND                            
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
*        FILTER ON KEY                                                          
*                                                                               
         CLI   WIOFKCDE,WIOFKIDQ   TEST FAX ELM CODE                            
         BNE   *+10                                                             
         CLC   WIOFKGP#,QFGP#      TEST GROUP NUMBER                            
         BNE   *+8                                                              
         CLI   WIOFKTYP,WIOFKFXQ   TEST FAX NUMBER ELEMENT                      
         BNE   *+8                                                              
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOFKFTP,WIOFKFAQ   SKIP IF NOT FAX ADDRESS ELM                  
         BNE   DRFAXNXT                                                         
*                                                                               
         TITLE 'T41E20 - FAX MAINT/LIST - DRFNAM'                               
***********************************************************************         
*                                                                     *         
*        ROUTINE TO DISPLAY  FAX RECIPIENT'S NAME                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRFNAM   DS    0H                                                               
*                                                                               
         BRAS  RE,CLRFLD           CLEAR THE FIELD                              
*                                                                               
         OC    WIOFXNAM,WIOFXNAM   SKIP IF NO NAME AVAILABLE                    
         BZ    DRFNAMX                                                          
*                                                                               
         MVC   FLDDATA(L'SFXNAM1),WIOFXNAM DISPLAY FAX NAME                     
*                                                                               
         BRAS  RE,SETLEN           FILL IN TRUE LENGTH OF DATA                  
*                                                                               
DRFNAMX  DS    0H                                                               
*                                                                               
         TITLE 'PPWIO20 - DISPLAY FAX NUMBER - DRFX#'                           
***********************************************************************         
*                                                                     *         
*        ROUTINE TO DISPLAY FAX NUMBER                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRFX#    DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO FAX NUMBER                           
*                                                                               
         BRAS  RE,CLRFLD           CLEAR THE FIELD                              
*                                                                               
         OC    WIOFX#,WIOFX#       SKIP IF NO NUMBER AVAILABLE                  
         BZ    DRFX#N                                                           
*                                                                               
         LA    RF,WIOFX#           COUNT DIGITS IN NUMBER                       
*                                                                               
         CLI   0(RF),C' '          FIND FIRST NON DIGIT                         
         BNH   *+12                                                             
         AHI   RF,1                BUMP TO NEXT POSITION                        
         B     *-12                                                             
*                                                                               
         LA    RE,WIOFX#                                                        
         SR    RF,RE               NUMBER OF DIGITS                             
*                                                                               
         CHI   RF,10               SPECIAL FORMATTING FOR 10 DIGITS             
         BE    DRFX#10                                                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),WIOFX#   DISPLAY FAX #                                
*                                                                               
         B     DRFX#OK                                                          
*                                                                               
DRFX#10  DS    0H                  FORMAT AS XXX-XXX-XXXX                       
*                                                                               
         MVC   FLDDATA(3),WIOFX#                                                
         MVI   FLDDATA+3,C'-'                                                   
         MVC   FLDDATA+4(3),WIOFX#+3                                            
         MVI   FLDDATA+7,C'-'                                                   
         MVC   FLDDATA+8(4),WIOFX#+6                                            
*                                                                               
         B     DRFX#OK                                                          
*                                                                               
*        DISPLAY E-MAIL ADDRESS                                                 
*                                                                               
DRFX#N   DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOFKLEN         GET ELEMENT LENGTH                           
         SHI   RF,WIOFFAXL         CALCULATE E-MAIL ADDR LENGTH                 
         BNP   DRFX#OK             NONE AVAILABLE                               
*                                                                               
         CHI   RF,L'SFXFAX1        MAKE SURE ADDRESS FITS                       
         BNH   *+8                                                              
         LHI   RF,L'SFXFAX1                                                     
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),WIOFFEML DISPLAY E-MAIL ADDRESS                       
*                                                                               
DRFX#OK  DS    0H                                                               
*                                                                               
         BRAS  RE,SETLEN           FILL IN TRUE LENGTH OF DATA                  
*                                                                               
DRFX#X   DS    0H                                                               
*                                                                               
         TITLE 'PPWIO20 - DISPLAY FAX TYPE   - DRFTYP'                          
***********************************************************************         
*                                                                     *         
*        ROUTINE TO DISPLAY  FAX TYPE                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRFTYP   DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO FAX STATUS                           
*                                                                               
         BRAS  RE,CLRFLD           CLEAR FIELD                                  
*                                                                               
         OC    WIOFXTYP,WIOFXTYP   SKIP IF NO TYPE AVAILABLE                    
         BZ    DRFTYPX                                                          
*                                                                               
         CLI   WIOFXTYP,C'F'       IF TYPE   IS FYI                             
         BNE   *+10                                                             
         MVC   FLDDATA(3),=C'FYI'                                               
*                                                                               
DRFTYPOK DS    0H                                                               
*                                                                               
         BRAS  RE,SETLEN           SET OUTPUT LENGTH                            
*                                                                               
DRFTYPX  DS    0H                                                               
*                                                                               
         TITLE 'PPWIO20 - DISPLAY SUPPRESS COSTS OPTION   - DRSCS'              
***********************************************************************         
*                                                                     *         
*        ROUTINE TO DISPLAY  SUPPRESS COSTS OPTION                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRSCS    DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO SUPPRESS COSTS FIELD                 
*                                                                               
         BRAS  RE,CLRFLD           CLEAR FIELD                                  
*                                                                               
         OC    WIOFXNAM,WIOFXNAM   SKIP IF NO NAME AVAILABLE                    
         BZ    DRSCSX                                                           
*                                                                               
         CLI   WIOFXSCS,C'Y'       IF OPTION IS YES                             
         BNE   *+14                                                             
         MVC   FLDDATA(1),=C'Y'                                                 
         B     DRSCSOK                                                          
*                                                                               
         MVC   FLDDATA(1),=C'N'    ELSE DEFAULT TO C'N'                         
*                                                                               
DRSCSOK DS     0H                                                               
*                                                                               
         BRAS  RE,SETLEN           SET OUTPUT LENGTH                            
*                                                                               
DRSCSX   DS    0H                                                               
*                                                                               
         TITLE 'PPWIO20 - DISPLAY FAX STATUS - DRFSTA'                          
***********************************************************************         
*                                                                     *         
*        ROUTINE TO DISPLAY  FAX STATUS                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRFSTA   DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO FAX STATUS                           
*                                                                               
         MVC   QSTAT,WIOFXSTA      SET STATUS TO BE DISPLAYED                   
*                                                                               
         GOTOR DISSTA              DISPLAY STATUS                               
*                                                                               
         SR    R3,R3               INIT POINTER                                 
*                                                                               
         OC    WIOFXSTA,WIOFXSTA   SKIP IF NO STATUS AVAILABLE                  
         BZ    DRFSTAX                                                          
*                                                                               
         CLI   WIOFXSTA,WIOSSNTQ   IF STATUS IS SENT                            
         BE    *+8                                                              
         CLI   WIOFXSTA,WIOSRSTQ   IF STATUS IS RE-SENT                         
         BNE   *+12                                                             
         LA    R3,WIOFXFDT            POINT TO FAXED DATE AND TIME              
         B     DRFSTAOK                                                         
*                                  ELSE                                         
         LA    R3,WIOFXDDT            POINT TO DELIVERD DATE AND TIME           
*                                                                               
DRFSTAOK DS    0H                                                               
*                                                                               
         BRAS  RE,SETLEN           SET OUTPUT LENGTH                            
*                                                                               
DRFSTAX  DS    0H                                                               
*                                                                               
         TITLE 'PPWIO20 - DISPLAY FAX DATE - DRFDTE'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO DISPLAY  FAX DATE                                 *         
*        R3 ==> DATE AND TIME                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRFDTE   DS    0H                                                               
*                                                                               
         BRAS  RE,BUMP             BUMP TO FAX DATE                             
*                                                                               
         BRAS  RE,CLRFLD           CLEAR FIELD                                  
*                                                                               
         LTR   R3,R3               SKIP IF NO POINTER AVAILABLE                 
         BZ    DRFDTEX                                                          
*                                                                               
         OC    0(3,R3),0(R3)       SKIP IF NO DATE AVAILABLE                    
         BZ    DRFDTEX                                                          
*                                                                               
         GOTOR DATCON,DMCB,(3,0(R3)),(17,FLDDATA)  DISPLAY DATE                 
*                                                                               
         BRAS  RE,SETLEN           SET OUTPUT LENGTH                            
*                                                                               
DRFDTEX  DS    0H                                                               
*                                                                               
         TITLE 'PPWIO20 - DISPLAY FAX TIME - DRFTIM'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO DISPLAY  FAX TIME                                 *         
*        R3 ==> DATE AND TIME                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRFTIM   DS    0H                                                               
*                                                                               
         BRAS  RE,BUMP             BUMP TO FAX DATE                             
*                                                                               
         BRAS  RE,CLRFLD           CLEAR FIELD                                  
*                                                                               
         LTR   R3,R3               SKIP IF NO POINTER AVAILABLE                 
         BZ    DRFTIMX                                                          
*                                                                               
         OC    3(3,R3),3(R3)       SKIP IF NO TIME AVAILABLE                    
         BZ    DRFTIMX                                                          
*                                                                               
         GOTOR DISTIM,DMCB,3(R3)   DISPLAY TIME                                 
*                                                                               
         BRAS  RE,SETLEN           SET OUTPUT LENGTH                            
*                                                                               
DRFTIMX  DS    0H                                                               
*                                                                               
         TITLE 'PPWIO20 - DISPLAY  FAX NUMBER - DRFAXCN'                        
***********************************************************************         
*                                                                     *         
*        CONTINUATION OF FAX PROCESSING LOOP                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRFAXCN  DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO NEXT LINE                            
*                                                                               
DRFAXNXT DS    0H                                                               
*                                                                               
         C     R6,MINELEM          IF PREVIOUS ELEMENT FOUND                    
         BNE   *+14                                                             
         MVC   ELEMENT(L'WIOFKEY),WIOFKEY COPY KEY                              
         LA    R6,ELEMENT          POINT TO KEY                                 
*                                                                               
         MVI   WIOFKLEN,WIOFKSQN-WIOFKEY  SET FILTER LENGTH                     
*                                                                               
         GOTOR NXTELM,DMCB,WIOFKEY   FIND NEXT ELEMENT                          
*                                                                               
DRFAXCN1 DS    0H                                                               
*                                                                               
         BCT   R0,DRFAXLP          PROCESS NEXT LINE                            
*                                                                               
DRFAXDN  DS    0H                                                               
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
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E20 - FAX MAINT/LIST - DK'                                   
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
*                                                                               
         TM    MINSTAT,MINDELQ     SKIP IF DELETED                              
         BO    DKX                                                              
*                                                                               
*        READ IN HEADER ELEMENT                                                 
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
         L     R1,MINELEM          POINT TO FOUND ELEMENT                       
         MVC   ELEMENT,0(R1)       SAVE HEADER ELEMENT                          
*                                                                               
         USING WIOHDRD,R6          ESTABLISH HEADER ELEMENT                     
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,SELLISTN       GET RELATIVE NUMBER IN LIST                  
         MHI   RF,L'SVLFXSFX       CALCULATE INDEX                              
         LA    RF,SVLFXSFX(RF)     INDEX TO ENTRY  IN TABLE                     
*                                                                               
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING WIOFKEY,R6          ESTABLISH FAX ELEMENT                        
*                                                                               
         MVC   WIOFKEY,0(RF)       SET DETAIL KEY FOR SELECTION                 
*                                                                               
DKDKYX   DS    0H                                                               
*                                                                               
         GOTO1 GETELM,DMCB,WIOFKEY FIND ELEMENT                                 
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
         LA    R2,SFXMEDH          POINT TO MEDIA FIELD                         
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         MVC   QMED,WIOKMED        SET MEDIA                                    
*                                                                               
         GOTOR DISMED              DISPLAY MEDIA                                
*                                                                               
*        SET CLIENT ON SCREEN                                                   
*                                                                               
         LA    R2,SFXCLTH          POINT TO CLIENT FIELD                        
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         MVC   QCLT,WIOKCLT        SET CLIENT                                   
*                                                                               
         GOTOR DISCLT              DISPLAY CLIENT                               
*                                                                               
*        SET PUB ON SCREEN                                                      
*                                                                               
         LA    R2,SFXPUBH          POINT TO PUB FIELD                           
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         MVC   QPUB,WIOKPUB        SET MASTER PUB                               
*                                                                               
         GOTOR DISPUB              DISPLAY PUB CODE                             
*                                                                               
*        SET INSORD NUMBER ON SCREEN                                            
*                                                                               
         LA    R2,SFXIO#H          POINT TO INSORD # FIELD                      
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         MVC   QIO#,WIOKIO#        SET INSORD #                                 
         MVC   QREV#,WIOKRV#       SET REVISION NUMBER                          
*                                                                               
         GOTOR DISIO#              DISPLAY IO#                                  
*                                                                               
         LA    R2,SFXREV#H         POINT TO REVISION # FIELD                    
*                                                                               
         GOTOR DISRV#              DISPLAY REVISION #                           
*                                                                               
*        SET FAX GROUP NUMBER ON SCREEN                                         
*                                                                               
         LA    R2,SFXFGP#H         POINT TO FAX GROUP NUMBER FIELD              
*                                                                               
         USING WIOFKEY,R6                                                       
         EDIT  WIOFKGP#,SFXFGP#,0,ALIGN=LEFT                                    
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
         TITLE 'T41E20 - FAX MAINT/LIST - DL'                                   
***********************************************************************         
*                                                                     *         
*        DELETE A FAX GROUP                                           *         
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
*        READ IN ALL RELATED FAX ELEMENTS AND DELETE                            
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT          ESTABLISH DETAIL ELEMENT KEY                 
         USING WIOFKEY,R6                                                       
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET ELEMENT ID                               
         MVC   WIOFKGP#,QFGP#      SET GROUP   NUMBER                           
         MVI   WIOFKLEN,WIOFKTYP-WIOFKEY   FILTER ON GROUP #                    
*                                                                               
         GOTO1 GETELM,DMCB,ELEMENT FIND FIRST ELEMENT                           
*                                                                               
         DROP  R6                                                               
*                                                                               
         USING WIOFKEY,SVHDRELM    ESTABLISH FAX ELEMENT                        
*                                                                               
DLFAXLP  DS    0H                                                               
*                                                                               
         BNZ   DLFAXDN             END OF FAX ELEMENTS                          
*                                                                               
         L     R1,MINELEM          POINT TO FOUND ELEMENT                       
         SR    RF,RF                                                            
         IC    RF,1(R1)            GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WIOFAXD(0),0(R1)    MOVE ELEMENT TO WORK AREA                    
*                                                                               
         GOTOR DELELM,DMCB,SVHDRELM DELETE ELEMENT                              
*                                                                               
         GOTOR GETELM,DMCB,SVHDRELM RE-SET   FILE POINTERS                      
*                                                                               
DLFAXCN  DS    0H                                                               
*                                                                               
         MVI   WIOFKLEN,WIOFKTYP-WIOFKEY   FILTER ON DETAIL SQN                 
*                                                                               
         GOTOR NXTELM,DMCB,SVHDRELM FIND NEXT ELEMENT                           
*                                                                               
         B     DLFAXLP                                                          
*                                                                               
DLFAXDN  DS    0H                                                               
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
         CLI   MINERR,0            MUST SUCCEED                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        CALL LINKIO INTERFACE IF NEEDED                                        
*                                                                               
         CLI   DDLNKSW,C'Y'        IF IN A LINK CALL                            
         BNE   DLLNKX                                                           
*                                                                               
         GOTOR LNKPUT,DMCB,(RC)       SEND DATA BACK TO CALLER                  
*                                                                               
DLLNKX   DS    0H                                                               
*                                                                               
DLX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E20 - FAX MAINT/LIST - SEND'                                 
***********************************************************************         
*                                                                     *         
*        SEND   A FAX GROUP                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SEND     NTR1  BASE=*,LABEL=*      DISPLAY RECORD                               
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
*        CHECK MANUAL SEND INDICATOR                                            
*                                                                               
         MVI   QMANSD,0            INIT INDICATOR                               
*                                                                               
         CLC   =C'MANUAL',LNKMANSD CHECK FOR MANUAL SEND                        
         BNE   *+8                                                              
         MVI   QMANSD,C'M'            SET INDICATOR                             
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
         BE    *+6                 NO ERRORS TOLERATED                          
         DC    H'0'                                                             
*                                                                               
         TM    MINSTAT,MINDELQ     SKIP IF DELETED                              
         BO    SENDX                                                            
*                                                                               
*        READ IN AND SEND ALL FAX ELEMENTS                                      
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          BUILD ELEMENT KEY                            
         USING WIOFKEY,R6                                                       
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET FAX ELM CODE                             
         MVC   WIOFKGP#,QFGP#      SET GROUP NUMBER                             
         MVI   WIOFKTYP,WIOFKFXQ   SET FOR FAX NUMBER ELEMENT                   
         MVI   WIOFKLEN,WIOFKSQN-WIOFKEY MATCH ON TYPE                          
*                                                                               
         GOTOR GETELM,DMCB,WIOFKEY FIND FIRST ELEMENT                           
*                                                                               
*                                                                               
SND1STLP DS    0H                                                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
*                                  DONE IF NOT FAX ELEMENT                      
         CLI   WIOFKCDE,WIOFKIDQ   TEST FAX ELM CODE                            
         BNE   *+10                                                             
         CLC   WIOFKGP#,QFGP#      TEST GROUP NUMBER                            
         BNE   *+8                                                              
         CLI   WIOFKTYP,WIOFKFXQ   TEST FAX NUMBER ELEMENT                      
         BNE   SNDFAXX                                                          
*                                                                               
         CLC   SVSCS,WIOFXSCS      MUST MATCH SUPPRESS COST OPTION              
         BE    SND1STFD                                                         
*                                                                               
SND1STCN DS    0H                                                               
*                                                                               
         MVC   ELEMENT(L'WIOFKEY),WIOFKEY  COPY KEY                             
         MVI   WIOFKLEN,WIOFKSQN-WIOFKEY MATCH ON TYPE                          
*                                                                               
         GOTO1 NXTELM,DMCB,ELEMENT  FIND NEXT ELEMENT ON FILE                   
*                                                                               
         B     SND1STLP                                                         
*                                                                               
SND1STFD DS    0H                                                               
*                                                                               
         MVC   ELEMENT,0(R6)       SAVE FOUND ELEMENT                           
*                                                                               
SNDFAX   DS    0H                                                               
*                                                                               
*        FAX NUMBER FOUND - PRINT HDR LINE                                      
*                                                                               
         BRAS  RE,OPENSPQ          OPEN THE PRINT QUEUE                         
*                                                                               
         LA    R3,P                                                             
         USING EDICTD,R3                                                        
         MVC   P,SPACES            INIT PRINT LINE                              
*                                                                               
         MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,0                                                           
*                                                                               
*        BUILD EDICT HEADER RECORD                                              
*        SEE H:FACDOC/EDICT.DOC  FOR DETAIL DOCUMENTATION                       
*                                                                               
         MVC   P+04(5),=C'*HDR*'   EDICT HEADER CARD                            
         MVC   P+09(3),=C'FAX'     THIS IS A FAX TRANSACTION                    
*                                    OUTPUT IS 11INCH/80 CHAR REPORT            
         CLI   WIOFX#+10,C' '      IF INTERNATIONAL NUMBER                      
         BNH   *+14                                                             
         MVC   P+13(16),WIOFX#        DON'T FORMAT                              
         B     SNDFX10                                                          
*                                                                               
         MVC   P+13(3),WIOFX#      FAX NUMBER                                   
         MVI   P+16,C'-'                                                        
         MVC   P+17(3),WIOFX#+3                                                 
         MVI   P+20,C'-'                                                        
         MVC   P+21(4),WIOFX#+6                                                 
*                                                                               
SNDFX10  DS    0H                                                               
*                                                                               
         MVI   P+35,C'P'           DROP EASILINK HEADER FROM FAX                
*                                                                               
         MVC   P+38(16),WIOFX#     DESTINATION                                  
*                                                                               
*        UPDATE STATUS OF FAX ELEMENT                                           
*                                                                               
         MVI   WIOFXSTA,WIOSSNTQ   SET SENT STATUS                              
*                                                                               
         GOTOR DATCON,DMCB,(5,0),(25,WIOFXFDT)   FAXED DATE/TIME                
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOFXFTM         CONVERT DDS HOURS                            
         AHI   RF,6                                                             
         STC   RF,WIOFXFTM                                                      
*                                                                               
         MVC   SVDATE,WIOFXFDT     SAVE DATE AND TIME                           
         MVC   SVTIME,WIOFXFTM                                                  
*                                                                               
         MVC   WIOFXPID,SVWIOPID    SET SENDER'S PID                            
*                                                                               
         GOTOR WRTELM,DMCB,WIOFKEY   UPDATE FAX ELEMENT                         
*                                                                               
SNDFX15  DS    0H                                                               
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT RE-POINT MINIO TO ELEMENT                    
*                                                                               
         MVI   LINE,1                                                           
         GOTOR SPOOL,DMCB,SPOOLD   PRINT A LINE                                 
*                                                                               
*        EDICT TRANSACTION CARD                                                 
*                                                                               
         MVC   P,SPACES            INIT PRINT LINE                              
*                                                                               
         MVC   P+00(5),=C'++DDS'   REQUIRED IDENTIFIER                          
         MVC   P+06(2),=C'PR'      PRINT SYSTEM                                 
         MVC   P+08(3),=C'ZAB'     ADBUYER FAX                                  
         MVC   P+11(3),=C'TRN'     TRANSACTION CARD                             
*                                                                               
*        FILL IN TRANSACTION IDENTIFIERS                                        
*                                                                               
         LA    R2,P+15             POINT TO TRANSACTION ID AREA                 
         USING PPEDAREA,R2         ESTABLISH TRANS ID AREA                      
*                                                                               
         MVC   PPEAMED,QMED        SET MEDIA                                    
         MVC   PPEACLT,QCLT        SET CLIENT                                   
         MVC   PPEAPRD,WIOHPRD-WIOHKEY+SVHDRELM SET PRODUCT                     
*                                                                               
         GOTO1 VPUBEDIT,DMCB,(0,QPUB),(C'S',PPEAPUB) DISPLAY PUB                
*                                                                               
         GOTOR FMTIO#              FORMAT LONG IO NUMBER                        
*                                                                               
         MVC   PPEAIO#(L'PPEAIO#-1),QIO#EXP  ASSUME 4 DIGIT SQN                 
         LA    R3,PPEAGRP-3        A(GROUP NUMBER FIELD)                        
*                                                                               
         CLC   QIO#IOSQ,=AL3(9999) IF OVER 10000                                
         BNH   SNDFX17                                                          
*                                                                               
         MVC   PPEAIO#(L'PPEAIO#),QIO#EXP  MOVE IN EXTRA BYTE                   
         MVI   PPEIND,C'I'            INDICATE 5 DIGIT SQN                      
         LA    R3,PPEAGRP             POINT TO FAX GROUP #                      
*                                                                               
SNDFX17  DS    0H                                                               
*                                                                               
         USING PPEAGRP,R3          ESTABLISH LAST FIELDS IN OUTPUT              
*                                                                               
         EDIT  WIOFKGP#,PPEAGRP,0  SET FAX GROUP NUMBER                         
*                                                                               
         MVC   PPEAAGY,QAGY        PASS AGENCY ALPHA                            
*                                                                               
         DROP  R3                                                               
*                                                                               
         MVI   LINE,1                                                           
         GOTOR SPOOL,DMCB,SPOOLD   PRINT A LINE                                 
*                                                                               
*        EDICT MQ CARD                                                          
*                                                                               
         MVC   P,SPACES            INIT PRINT LINE                              
*                                                                               
         MVC   P+00(5),=C'++DDS'   REQUIRED IDENTIFIER                          
         MVC   P+11(3),=C'MQN'     TRANSACTION CARD                             
*                                                                               
         MVC   P+15(3),SSYSNA      PRINT SYSTEM NAME                            
*                                                                               
         MVC   P+24(6),=C'PRTADB'  IDENTIFY AS COMING FROM ADBUYER              
*                                                                               
         MVI   LINE,1                                                           
         GOTOR SPOOL,DMCB,SPOOLD   PRINT A LINE                                 
*                                                                               
*        COPY FAX TO PRINT QUEUE CARD - IF NEEDED                               
*                                                                               
         CLI   LNKCPYFX,C'Y'       SKIP IF NOT COPYING FAX TO PQ                
         BNE   SNDFXCPX                                                         
*                                                                               
         MVC   P,SPACES            INIT PRINT LINE                              
*                                                                               
         MVC   P+00(5),=C'++DDS'   REQUIRED IDENTIFIER                          
         MVC   P+11(3),=C'PQS'     PQ STATUS CHANGE                             
*                                                                               
         MVI   P+15,C'Z'           PRINT CLASS Z                                
*                                                                               
         MVI   LINE,1                                                           
         GOTOR SPOOL,DMCB,SPOOLD   PRINT A LINE                                 
*                                                                               
SNDFXCPX DS    0H                                                               
*                                                                               
*        LOOP TO FORMAT DESTINATION CARDS FOR OTHER FAX #'S                     
*                                                                               
*        READ IN FAX NUMBER ELEMENTS AND SEND FAX NUMBERS                       
*                                                                               
         USING WIOFKEY,R6          ESTABLISH FAX ELEMENT KEY                    
*                                                                               
         GOTO1 NXTELM,DMCB,WIOFAXD FIND NEXT  ELEMENT ON FILE                   
*                                                                               
SNDFAXLP DS    0H                                                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
*                                  DONE IF NOT FAX ELEMENT                      
*                                                                               
         CLI   WIOFKCDE,WIOFKIDQ   TEST FAX ELM CODE                            
         BNE   *+10                                                             
         CLC   WIOFKGP#,QFGP#      TEST GROUP NUMBER                            
         BNE   *+8                                                              
         CLI   WIOFKTYP,WIOFKFXQ   TEST FAX NUMBER ELEMENT                      
         BNE   SNDFAXDN                                                         
*                                                                               
         CLI   WIOFKFTP,WIOFKFAQ   SKIP IF NOT FAX ADDRESS ELM                  
         BNE   SNDFAXCN                                                         
*                                                                               
         OC    WIOFX#,WIOFX#       SKIP IF NO FAX # AVAILABLE                   
         BZ    SNDFAXCN                                                         
*                                                                               
         CLC   SVSCS,WIOFXSCS      SKIP IF NOT WANTED SUPPRESS                  
         BNE   SNDFAXCN                COST OPTION                              
*                                                                               
*        SEND EDICT A FAX NUMBER                                                
*                                                                               
         MVC   P,SPACES            INIT PRINT LINE                              
*                                                                               
         MVC   P+00(5),=C'++DDS'   DDS DATA INDICATOR                           
         MVC   P+11(3),=C'DST'     DESTINATION CARD                             
         MVC   P+15(3),=C'FAX'     THIS IS A FAX TRANSACTION                    
*                                                                               
         CLI   WIOFX#+10,C' '      IF INTERNATIONAL NUMBER                      
         BNH   *+14                                                             
         MVC   P+19(16),WIOFX#        DON'T FORMAT                              
         B     SNDFAX10                                                         
*                                                                               
         MVC   P+19(3),WIOFX#      FAX NUMBER                                   
         MVI   P+22,C'-'                                                        
         MVC   P+23(3),WIOFX#+3                                                 
         MVI   P+26,C'-'                                                        
         MVC   P+27(4),WIOFX#+6                                                 
*                                                                               
SNDFAX10 DS    0H                                                               
*                                                                               
         MVC   P+41(16),WIOFX#     DESTINATION                                  
*                                                                               
         MVI   LINE,1                                                           
         GOTOR SPOOL,DMCB,SPOOLD   PRINT A LINE                                 
*                                                                               
*        UPDATE STATUS OF FAX ELEMENT                                           
*                                                                               
         MVI   WIOFXSTA,WIOSSNTQ   SET SENT STATUS                              
*                                                                               
         MVC   WIOFXFDT,SVDATE      SET FAX DATE AND TIME                       
         MVC   WIOFXFTM,SVTIME                                                  
*                                                                               
         MVC   WIOFXPID,SVWIOPID    SET SENDER'S PID                            
*                                                                               
         MVC   ELEMENT,0(R6)       SAVE FAX ELEMENT                             
*                                                                               
         GOTOR WRTELM,DMCB,WIOFKEY   UPDATE FAX ELEMENT                         
*                                                                               
SNDFAX90 DS    0H                                                               
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT RE-POINT MINIO TO ELEMENT                    
*                                                                               
SNDFAXCN DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,MINELEM  FIND NEXT ELEMENT                           
*                                                                               
         B     SNDFAXLP                                                         
*                                                                               
SNDFAXDN DS    0H                                                               
*                                                                               
*        RETRIEVE FAX TEXT LINES AND SEND TO EDICT                              
*                                                                               
         CLI   DDLNKSW,C'Y'        SKIP IF NOT IN A LINK CALL                   
         BNE   SNDTXTDN                                                         
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)                                                      
         USING LIOBD,R3            ESTABLISH LINKIO INTERFACE BLOCK             
*                                                                               
         TM    LIOBFLG2,LIOBFEOF   TEST FOR EOF ALREADY FOUND                   
         BO    SNDTXTDN                                                         
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAGET',LIOBD)  GET LINE OF TEXT                 
*                                                                               
SNDTXTLP DS    0H                                                               
*                                                                               
         TM    LIOBFLG2,LIOBFEOF   TEST FOR EOF FOUND                           
         BO    SNDTXTDN                                                         
*                                                                               
         USING WKRDATD,R2          ESTABLISH GENERIC WORKER FILE ELM            
*                                                                               
         ICM   R2,15,LNKAFXLN      POINT TO FAX TEXT                            
         BZ    SNDTXTCN               NO TEXT                                   
*                                                                               
         CLI   WKDTRID,0           DONE AT END OF RECORDS                       
         BE    SNDTXTCN                                                         
*                                                                               
         CLC   WKDTMPCD,=AL2(D#LINTXT)  DONE IF NOT FAX TEXT                    
         BNE   SNDTXTCN                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WKDTRLEN       GET ELEMENT LENGTH                           
         LR    R0,RF               SAVE ELEMENT LENGTH                          
*                                                                               
         SHI   RF,WKDTHDLQ         SUBTRACT HEADER LENGTH                       
         BNZ   SNDTXT10            GOT DATA                                     
*                                                                               
         MVC   P,SPACES            ELSE FORCE A BLANK LINE                      
*                                                                               
         B     SNDTXT20                                                         
*                                                                               
SNDTXT10 DS    0H                                                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),WKDTDATA       MOVE TEXT TO PRINT LINE                      
*                                                                               
SNDTXT20 DS    0H                                                               
*                                                                               
         MVI   LINE,1                                                           
         GOTOR SPOOL,DMCB,SPOOLD   PRINT A LINE                                 
*                                                                               
SNDTXTCN DS    0H                                                               
*                                                                               
         MVC   P,SPACES            CLEAR PRINT LINE                             
         XC    LNKAFXLN,LNKAFXLN   CLEAR LAST ADDRESS                           
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAGET',LIOBD)  GET LINE OF TEXT                 
*                                                                               
         B     SNDTXTLP                                                         
*                                                                               
SNDTXTDN DS    0H                                                               
*                                                                               
*        SET FAX HEADER STATUS TO SENT                                          
*                                                                               
         XC    ELEMENT,ELEMENT     INIT GROUP HEADER ELEMENT                    
         LA    R6,ELEMENT          ESTABLISH GROUP HEADER ELEMENT               
         USING WIOFAXD,R6                                                       
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET AS FAX ELEMENT                           
         MVC   WIOFKGP#,QFGP#      SET GROUP NUMBER                             
         MVI   WIOFKTYP,WIOFKHDQ   SET AS GROUP HEADER                          
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT  READ HEADER ELEMENT                         
         BZ    *+6                 FOUND                                        
         DC    H'0'                 SHOULD BE THERE                             
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         MVC   WIOFSTAT,SVSTAT     INDICATE FAX SENT/RESENT                     
*                                                                               
         MVC   WIOFSDTE,SVDATE      SET FAX DATE AND TIME                       
         MVC   WIOFSTIM,SVTIME                                                  
*                                                                               
         MVC   WIOFHPID,SVWIOPID    SET SENDER'S PID                            
*                                                                               
         GOTOR WRTELM,DMCB,WIOFKEY    ELEMENT TO RECORD                         
         BZ    *+6                 MUST SUCCEED                                 
         DC    H'0'                                                             
*                                                                               
*        SET IO HEADER STATUS TO SENT                                           
*                                                                               
         XC    ELEMENT,ELEMENT     INIT IO HEADER ELEMENT                       
         LA    R6,ELEMENT          ESTABLISH IO HEADER ELEMENT                  
         USING WIOHDRD,R6                                                       
*                                                                               
         MVI   WIOHKCDE,WIOHKIDQ   SET AS HEADER ELEMENT                        
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT  READ HEADER ELEMENT                         
         BZ    *+6                 FOUND                                        
         DC    H'0'                 SHOULD BE THERE                             
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         MVC   WIOHMSND,QMANSD     PASS ON MANUAL SENT INDICATOR                
*                                                                               
         CLI   WIOHSTAT,WIOSGENQ   IF CURRENT STATUS IS GENERATED               
         BNE   *+14                                                             
         MVC   WIOHSTAT,SVSTAT        USE STATUS OF TRANSACTION                 
         B     SNDSTA20                                                         
*                                                                               
         CLI   WIOHSTAT,WIOSSNTQ   IF CURRENT STATUS IS SENT                    
         BE    *+8                                                              
         CLI   WIOHSTAT,WIOSUDLQ   IF CURRENT STATUS IS UNDELIVERED             
         BE    *+8                                                              
         CLI   WIOHSTAT,WIOSRSTQ   IF CURRENT STATUS IS RESENT                  
         BNE   *+14                                                             
         MVC   WIOHSTAT,SVSTAT        USE TRANSACTION STATUS                    
         B     SNDSTA20                                                         
*                                                                               
         B     SNDSTA30            ELSE LEAVE HEADER STATUS AS IS               
*                                                                               
SNDSTA20 DS    0H                                                               
*                                                                               
         GOTOR WRTELM,DMCB,WIOHKEY    ELEMENT TO RECORD                         
         BZ    *+6                 MUST SUCEED                                  
         DC    H'0'                                                             
*                                                                               
SNDSTA30 DS    0H                                                               
*                                                                               
*        ADD NEW STATUS ELEMENT                                                 
*                                                                               
         MVC   QSTAT,WIOHSTAT      SAVE HEADER STATUS                           
*                                                                               
         XC    SVSTAELM,SVSTAELM   INIT STATUS ELM SAVEAREA                     
*                                                                               
         XC    ELEMENT,ELEMENT     INIT STATUS ELEMENT                          
         LA    R6,ELEMENT          ESTABLISH STATUS ELEMENT                     
         USING WIOSTATD,R6                                                      
*                                                                               
*        FIND LATEST STATUS                                                     
*                                                                               
         MVI   WIOSKCDE,WIOSKIDQ   SET AS STATUS ELEMENT                        
         MVI   WIOSKLEN,WIOSKSQN-WIOSKEY  FILTER ON ALL STATUS ELMS             
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT  READ FIRST STATUS ELEMENT                   
         BNZ   SNDSTADN              NONE FOUND                                 
*                                                                               
SNDSTALP DS    0H                                                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOSKCDE,WIOSKIDQ   MUST BE STATUS ELEMENT                       
         BNE   SNDSTADN                                                         
*                                                                               
         MVC   SVSTAELM,WIOSKEY    SAVE STATUS ELEMENT                          
*                                                                               
SNDSTACN DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOSKEY NEXT STATUS ELEMENT                          
*                                                                               
         B     SNDSTALP                                                         
*                                                                               
SNDSTADN DS    0H                                                               
*                                                                               
         LA    R6,SVSTAELM         POINT TO LATEST STATUS ELEMENT               
*                                                                               
         SR    R0,R0                                                            
         IC    R0,WIOSKSQN         GET LATEST SQN                               
         AHI   R0,1                BUMP SQN                                     
*                                                                               
         LA    R6,ELEMENT          POINT TO ELEMENT BUILD AREA                  
*                                                                               
         MVI   WIOSKLEN,WIOSDRLQ   SET STATUS ELEMENT LENGTH                    
         STC   R0,WIOSKSQN         SET SEQUENCE NUMBER                          
*                                                                               
         MVI   WIOSSTAT,WIOSRSTQ   INDICATE IO RESENT                           
*                                                                               
         CLI   QSTAT,WIOSGENQ      IF HEADER STATUS IS GENERATED                
         BE    *+8                                                              
         CLI   QSTAT,WIOSSNTQ      IF HEADER STATUS IS SENT                     
         BNE   *+8                                                              
         MVI   WIOSSTAT,WIOSSNTQ      INDICATE IO SENT                          
*                                                                               
         MVC   WIOSDATE,SVDATE      SET FAX DATE AND TIME                       
         MVC   WIOSTIME,SVTIME                                                  
*                                                                               
         MVC   WIOSPID,SVWIOPID    SET USER'S PID                               
*                                                                               
         GOTOR ADDELM,DMCB,WIOSKEY    ADD ELEMENT TO RECORD                     
         BZ    *+6                 MUST SUCEED                                  
         DC    H'0'                                                             
*                                                                               
SNDFAXX  DS    0H                                                               
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
         CLI   MINERR,0            MUST SUCCEED                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SNDFAXXX DS    0H                                                               
*                                                                               
*        RETURN DATA TO ADBUYER IF NECESSARY                                    
*                                                                               
         CLI   DDLNKSW,C'Y'        IF NOT A LINKIO CALL                         
         BE    SNDLNK                                                           
*                                                                               
*        PRINT SOME FILLER LINES                                                
*                                                                               
         MVC   P(32),=CL32'THAT''S ALL FOLKS.'                                  
         MVI   LINE,1                                                           
         GOTOR SPOOL,DMCB,SPOOLD   PRINT A LINE                                 
*                                                                               
         MVC   P(32),=CL32'THAT''S ALL FOLKS.'                                  
         MVI   LINE,1                                                           
         GOTOR SPOOL,DMCB,SPOOLD   PRINT A LINE                                 
*                                                                               
         MVC   P(32),=CL32'THAT''S ALL FOLKS.'                                  
         MVI   LINE,1                                                           
         GOTOR SPOOL,DMCB,SPOOLD   PRINT A LINE                                 
*                                                                               
         MVC   P(32),=CL32'THAT''S ALL FOLKS.'                                  
         MVI   LINE,1                                                           
         GOTOR SPOOL,DMCB,SPOOLD   PRINT A LINE                                 
*                                                                               
         MVC   P(32),=CL32'THAT''S ALL FOLKS.'                                  
         MVI   LINE,1                                                           
         GOTOR SPOOL,DMCB,SPOOLD   PRINT A LINE                                 
*                                                                               
         MVC   P(32),=CL32'THAT''S ALL FOLKS.'                                  
         MVI   LINE,1                                                           
         GOTOR SPOOL,DMCB,SPOOLD   PRINT A LINE                                 
*                                                                               
         B     SNDLNKX                                                          
*                                                                               
SNDLNK   DS    0H                                                               
*                                                                               
*        MAKE SURE WE HAVE READ TO THE END OF THE FILE                          
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)         JUST IN CASE                                 
         USING LIOBD,R3            ESTABLISH LINKIO INTERFACE BLOCK             
*                                                                               
         TM    LIOBFLG2,LIOBFEOF   TEST FOR EOF ALREADY FOUND                   
         BO    SNDLNK2                                                          
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAGET',LIOBD) GET END OF DATA                   
*                                                                               
         TM    LIOBFLG2,LIOBFEOF   MUST BE EOF                                  
         BO    *+6                                                              
         DC    H'0'                ALL DATA FIELDS SHOULD BE PROC'D             
*                                                                               
SNDLNK2  DS    0H                                                               
*                                                                               
*        SEND RECORD CODE                                                       
*                                                                               
         LHI   R0,E#IOFXRP         GET REPLY CODE                               
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',(R0))                  
*                                                                               
*        SEND IO KEY                                                            
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#IOLKEY),    X        
               ('LD_CHARQ',QIO#EXP),(L'QIO#EXP,0)                               
*                                                                               
*        RETURN IO STATUS DATE                                                  
*                                                                               
         GOTOR DATCON,DMCB,(3,SVDATE),(17,WORK)                                 
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#STATDT),    X        
               ('LD_CHARQ',WORK),(8,0)                                          
*                                                                               
*        RETURN IO STATUS TIME                                                  
*                                                                               
         LA    R2,FLDH                                                          
         GOTOR DISTIM,DMCB,SVTIME  DISPLAY TIME                                 
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#STATTM),    X        
               ('LD_CHARQ',FLD),(8,0)                                           
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOACLO',LIOBD) CLOSE WORKER FILE                 
*                                                                               
SNDLNKX  DS    0H                                                               
*                                                                               
SENDX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E20 - FAX MAINT/LIST - OPENSPQ'                              
***********************************************************************         
*                                                                     *         
*        OPENSPQ - SET UP SPOOL KEY AND OPEN PRINT QUE                *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
OPENSPQ  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         MVC   REMUSER(3),=C'WIO'  SET USER ID                                  
*                                                                               
         CLI   LNKCPYFX,C'Y'       IF COPYING TO PQ                             
         BNE   *+10                                                             
         MVC   REMUSER(3),=C'EIO'     SET NEW USER ID                           
*                                                                               
         LA    R1,SPOOLKEY                                                      
         USING PQPLD,R1                                                         
*                                                                               
         XC    SPOOLKEY,SPOOLKEY                                                
         OI    SPOOLIND,SPUINIT                                                 
         MVC   PLSUBID,=CL3'SJR'                                                
         MVC   PLDESC,=CL11'WEBIO FAX'                                          
*                                                                               
         MVI   PLCLASS,C'G'                                                     
*                                                                               
         OI    GENSTAT3,NOCLRSPK   SPOOLKEY IS SET                              
*                                                                               
         DROP  R1                                                               
*                                                                               
         GOTOR OPENPQ                                                           
*                                                                               
OPENPQX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E20 - FAX MAINT/LIST - PR'                                   
***********************************************************************         
*                                                                     *         
*        PR - PRINT A REPORT                                          *         
*                                                                     *         
***********************************************************************         
*                                                                               
PR       NTR1  BASE=*,LABEL=*      PRINT RECORDS                                
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         BRAS  RE,VK               VALIDATE KEY                                 
*                                                                               
         BRAS  RE,FILLSCR          FILL IN FAX NUMBERS                          
*                                                                               
         BRAS  RE,VR               VALIDATE INPUT                               
*                                                                               
         BRAS  RE,SEND                                                          
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
         LTORG                                                                  
*                                                                               
PF12TXT  DC    C'PF12=RETURN/NEXTSEL'                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
SVFAXTBL EQU   256                 LENGTH OF TABLE ENTRY                        
SVFAXTB  DS    20XL(SVFAXTBL)    SAVEAREA FOR FAX ELEMENTS                      
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
         TITLE 'T41E20 - FAX MAINT/LIST - LISTLIND'                             
***********************************************************************         
*                                                                     *         
*        DSECT FOR A LIST SCREEN LINE                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LISTLIND DSECT                                                                  
LISTLIN  DS    0XL(L'LFXLIN1)      LIST SCREEN LINE                             
LSFGP#   DS    CL3                 GROUP NUMBER                                 
         DS    CL2                                                              
LSENAM   DS    CL23                SENDER'S NAME                                
         DS    CL2                                                              
LSGSTA   DS    CL10                GROUP STATUS                                 
         DS    CL2                                                              
LSGDTE   DS    CL8                 GROUP STATUS DATE                            
         DS    CL2                                                              
LSGTIM   DS    CL8                 GROUP STATUS TIME                            
*                                                                               
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
       ++INCLUDE PPWIOFCD          FAX MAINT SCREEN                             
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPWIOFAD          DETAIL LIST SCREEN                           
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
CHGSWTCH DS    CL1                 C'Y' - A KEY FIELD HAS BEEN CHGD             
CHGMED   EQU   X'80'               AGENCY CHANGED                               
CHGCLT   EQU   X'40'               CLIENT CHANGED                               
CHGPUB   EQU   X'20'               PUB CHANGED                                  
CHGIO#   EQU   X'10'               INSORD    NUMBER CHANGED                     
CHGRV#   EQU   X'08'               REVISION  NUMBER CHANGED                     
CHGFGP#  EQU   X'04'               FAX GROUP NUMBER CHNAGED                     
*                                                                               
WRTSW    DS    XL1                 X'FF' - DON'T UPDATE THE FILE                
*                                  WEB SERVER HAS SENT DATA FIRST               
*                                                                               
DATAFLDS DS    XL1                 KEY FIELDS WITH DATA                         
DFLCLTQ  EQU   X'80'               CLIENT                                       
DFLPUBQ  EQU   X'40'               PUB                                          
DFLIO#Q  EQU   X'20'               IO#                                          
DFLRV#Q  EQU   X'10'               REVISION #                                   
DFLPERQ  EQU   X'08'               PERIOD                                       
DFLFGP#Q EQU   X'04'               FAX GROUP NUMBER                             
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
SVEMLELM DS    XL256               E-MAIL ELEMENT SAVEAREA                      
SVFAXELM DS    0XL256              FAX    ELEMENT SAVEAREA                      
SVSTAELM DS    XL256               STATUS ELEMENT SAVEAREA                      
SVIOKEY  DS    XL(L'WIOKEY)        INSORD KEY SAVEAREA FOR LIST                 
*                                                                               
SVLFXSFX DS    16XL(L'WIOFKEY)     DETAIL KEYS FOR LIST                         
SVLFXLNQ EQU   *-SVLFXSFX          TABLE LENGTH                                 
*                                                                               
SVDATE   DS    XL3                 TODAY'S DATE                                 
SVTIME   DS    XL3                 TIME - HMS - BINARY                          
*                                                                               
LRLASTSW DS    XL1                 C'Y' - END OF DETAILS                        
*                                                                               
ACTSW    DS    XL1                 C'N' - NEW ACTIVITY ELEMENT                  
*                                  C'O' - OLD ACTIVITY ELEMENT                  
*                                                                               
SVSCS    DS    XL1                 SAVE SUPPRESS COST OPTION                    
SVSTAT   DS    XL1                 SAVE GROUP STATUS                            
*                                                                               
WGRPSW   DS    C                   C'N' - NEW GROUP BEING PROCESSED             
*                                                                               
         ORG                                                                    
*                                                                               
** DDCOMFACS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
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
         PRINT ON                                                               
*PRGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE         PRINT SYSTEM RECORD LAYOUTS                  
         PRINT ON                                                               
*PREDICT                                                                        
         PRINT OFF                                                              
       ++INCLUDE PPEDICT           PRINT SYSTEM EDICT DATA                      
         PRINT ON                                                               
** DDPERVALD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD         PERVAL CONTROL BLOCKALD                      
         PRINT ON                                                               
*DDMINBLK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMINBLK          MINIO CONTROL BLOCK                          
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*                                                                               
*DMPRTQL                                                                        
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL           PRINT QUEUE PRINT LINE                       
         PRINT ON                                                               
*PPERREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPERREQUS         PRINT SYSTEM ERROR EQUATES                   
         EJECT                                                                  
*                                                                               
*PPMAPEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPMAPEQUS         PRINT SYSTEM MAP CODE EQUATES                
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
**PAN#1  DC    CL21'015PPWIO20   12/17/07'                                      
         END                                                                    
