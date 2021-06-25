*          DATA SET CTSFM59    AT LEVEL 160 AS OF 06/04/15                      
*PHASE TA0A59A                                                                  
         TITLE 'TA0A59  DARE VENDOR RECORD'                                     
***********************************************************************         
*                                                                     *         
*  TITLE        TA0A59 - DARE VENDOR RECORD MAINT/LIST/REPORT         *         
*                                                                     *         
*  CALLED FROM  GENCON VIA TA0A00 (SFM CTFILE CONTROLLER)             *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, CHANGE, LIST, REPORT           *         
*                                                                     *         
*  INPUTS       SCREEN TA0A98 (MAINTENANCE)                           *         
*               SCREEN TA0A99 (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      UPDATED DARE VENDOR RECORD                            *         
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
***********************************************************************         
*                                                                               
         TITLE 'TA0A59  DARE VENDOR RECORD - INIT'                              
TA0A59   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0A59                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         OI    GENSTAT4,NODELLST   DELETE FROM LIST NOT ALLOWED                 
         OI    GENSTAT2,DISTHSPG   STAY ON SAME PAGE OF LIST                    
*                                                                               
         LA    RF,PAGE#Q           NUMBER OF ENTRIES ON A PAGE                  
         MHI   RF,2                NUMBER OF ENTRIES ON A SCREEN                
         CVD   RF,SCRN#            NUMBER OF ENTRIES ON A SCREEN                
*                                                                               
CKMODE   DS    0H                                                               
*                                                                               
         CLI   MODE,PROCPFK        PROCESS PF KEYS                              
         BNE   CKMODE1                                                          
*                                                                               
         CLI   ACTNUM,ACTSEL        SKIP IF NOT SELECT FROM LIST                
         BNE   CKMODE1                                                          
*                                                                               
         CLI   PFKEY,12            IF PFKEY 12 HIT                              
         BNE   *+16                                                             
         OI    GENSTAT2,NEXTSEL          GO TO NEXT SELECT                      
         NI    GENSTAT2,X'FF'-RETEQSEL   GO BACK TO LIST SCREEN                 
         B     CKMODEX                                                          
*                                                                               
CKMODE1  DS    0H                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   *+12                                                             
         BRAS  RE,VK                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+12                                                             
         BRAS  RE,VR                                                            
         B     CKMODE90                                                         
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BRAS  RE,DK                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+12                                                             
         BRAS  RE,DR                                                            
         B     CKMODE90                                                         
*                                                                               
         CLI   MODE,XRECADD        ID FILE CHANGED-RECORD ADDED                 
         BE    *+8                                                              
         CLI   MODE,XRECPUT           RECORD CHANGED                            
         BE    *+8                                                              
         CLI   MODE,XRECPUT           RECORD DELETED                            
         BE    *+8                                                              
         CLI   MODE,XRECPUT           RECORD RESTORED                           
         BE    *+8                                                              
         B     CKMODE20                                                         
*                                                                               
         OI    GENSTAT2,USGETTXT+USMYOK USE GETTXT AND MY MSG                   
*                                                                               
         LA    RE,GETTXTCB                                                      
         USING GETTXTD,RE                                                       
*                                                                               
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         MVI   GTMTYP,GTMINF       INFORMATIONAL MESSAGE                        
*                                                                               
         LA    RF,RECYCLEQ         RECYCLE MESSAGE                              
         STCM  RF,3,GTMSGNO        SET ERROR CODE                               
*                                                                               
         B     CKMODEX                                                          
*                                                                               
         DROP  RE                                                               
*                                                                               
CKMODE20 DS    0H                                                               
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   *+12                                                             
         BRAS  RE,LR                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,RECDEL                                                      
         BNE   CKMODEX                                                          
*                                                                               
         LA    R2,CONACTH           NO DELETES                                  
         MVI   ERROR,INVRCACT                                                   
         GOTOR ERREX                                                            
*                                                                               
CKMODE90 DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTSEL        SKIP IF NOT SELECT FROM LIST                
         BNE   CKMODEX                                                          
*                                                                               
         CLI   PFKEY,0             IF NO PF KEY HIT                             
         BNZ   CKMODE95                                                         
*                                                                               
         BRAS  RE,TSTNTRY             TEST IF ANY FIELD ENTERED                 
         BZ    *+12                   NO                                        
         OI    GENSTAT2,RETEQSEL      YES RETURN TO THIS SCREEN                 
         B     CKMODE95                                                         
*                                                                               
         OI    GENSTAT2,NEXTSEL          GO TO NEXT SELECT                      
*                                                                               
CKMODE95 DS    0H                                                               
*                                                                               
CKMODEX  DS    0H                                                               
         B     EXIT                                                             
*                                                                               
INITERR  DS    0H                                                               
         LA    R2,CONACTH           SECURITY LOCKOUT                            
         MVI   ERROR,SECLOCK                                                    
         GOTOR ERREX                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RECYCLEQ EQU   164                 CALL COMPUTER ROOM TO RECYCLE                
*                                                                               
         TITLE 'TA0A59  DARE VENDOR RECORD - VL'                                
***********************************************************************         
*                                                                     *         
*     VALIDATE KEY ROUTINE                                            *         
*                                                                     *         
***********************************************************************         
*                                                                               
VK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SVCTEVKY,SVCTEVKY        CLEAR                                   
         LA    R4,SVCTEVKY                                                      
*                                                                               
         USING CTEVKEY,R4          ESTABLISH VENDOR RECORD KEY                  
*                                                                               
         MVI   CTEVKTYP,CTEVKTYQ   SET RECORD MAJOR TYPE                        
         MVI   CTEVKSTY,CTEVKSTQ   SET RECORD SECONDARY TYPE                    
*                                                                               
*        VALIDATE VENDOR FIELD                                                  
*                                                                               
VKVNDR   DS    0H                                                               
*                                                                               
         XC    SVVNDR,SVVNDR       INIT VENDOR SAVEAREA                         
*                                                                               
         LA    R2,SCRVNDRH         POINT TO VENDOR FIELD                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          IF NO ENTRY                                  
         BNZ   VKVNDR10                                                         
*                                                                               
         CLI   ACTNUM,ACTLIST         OK IF LIST                                
         BE    VKVNDRX                                                          
*                                                                               
         B     VKMISS                 ELSE MISSING INPUT ERROR                  
*                                                                               
VKVNDR10 DS    0H                                                               
*                                                                               
         CHI   RF,L'CTEVKVND       MUST BE LESS THAN MAX LENGTH                 
         BH    VKLESSER                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVVNDR(0),8(R2)     SAVE VENDOR CODE                             
*                                                                               
         OC    SVVNDR,SPACES       FORCE UPPERCASE                              
*                                                                               
         MVC   CTEVKVND,SVVNDR     ADD VENDOR TO KEY                            
*                                                                               
         MVC   KEY,SVCTEVKY        SET STARTING KEY                             
*                                                                               
         GOTOR HIGH                READ FOR RECORD ON FILE                      
*                                                                               
         LA    R4,KEY              SWITCH KEY POINTER                           
*                                                                               
         CLC   KEY(CTEVKVND-CTEVKEY),SVCTEVKY CHECK IF VENDOR FOUND             
         BNE   VKVNDRNF                                                         
*                                                                               
         CLC   CTEVKVND,SVVNDR     MATCH ON INPUT VENDOR                        
         BNE   VKVNDRNF                                                         
*                                                                               
         CLI   CTEVKARS,0          MUST BE MASTER VENDOR RECORD                 
         BE    VKVNDRFD                                                         
*                                                                               
VKVNDRNF DS    0H                  VENDOR NOT ON FILE                           
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING NEW VENDOR                         
         BNE   VKVNDR90                                                         
*                                                                               
         CLI   SCRPTRH+5,0            PTR CODE MUST BE NOT ENTERED              
         BNE   VKNOFLD                                                          
         CLI   SCRSTAH+5,0            STA CODE MUST BE NOT ENTERED              
         BNE   VKNOFLD                                                          
         CLI   SCRAGYH+5,0            AGY CODE MUST BE NOT ENTERED              
         BNE   VKNOFLD                                                          
         CLI   SCROFCH+5,0            OFFICE MUST NOT BE ENTERED                
         BNE   VKNOFLD                                                          
*                                                                               
         B     VKVNDR90            ELSE OKAY                                    
*                                                                               
VKVNDRFD DS    0H                  RECORD FOUND ON FILE                         
*                                                                               
         MVC   SVVNDR,CTEVKVND     SAVE FULL VENDOR CODE                        
*                                                                               
VKVNDR90 DS    0H                                                               
*                                                                               
         LA    R4,SVCTEVKY         SWITCH KEY POINTER                           
*                                                                               
         MVC   CTEVKVND,SVVNDR     ADD VENDOR TO KEY                            
*                                                                               
         LA    RF,L'SVVNDR         VENDOR LENGTH                                
         FOUT  SCRVNDRH,SVVNDR,(RF)                                             
*                                                                               
VKVNDRX  DS    0H                                                               
*                                                                               
         XC    SVARS,SVARS         INIT RECORD TYPE                             
*                                                                               
*        VALIDATE PARTNER CODE                                                  
*                                                                               
VKPTR    DS    0H                                                               
*                                                                               
         LA    R4,SVCTEVKY         POINT TO SAVED KEY                           
*                                                                               
         XC    SVPTR,SVPTR         INIT PARTNER CODE SAVEAREA                   
*                                                                               
         LA    R2,SCRPTRH          POINT TO PARTNER CODE FIELD                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          OKAY IF NO ENTRY                             
         BZ    VKPTRX                                                           
*                                                                               
         CHI   RF,L'CTEVKUSC       MUST BE LESS THAN MAX LENGTH                 
         BH    VKLARGE                                                          
*                                                                               
         CHI   RF,2                MUST BE AT LEAST 2 CHARACTERS                
         BL    VKLESSER                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVPTR(0),8(R2)      SAVE PARTNER CODE                            
*                                                                               
         OC    SVPTR,SPACES        FORCE UPPERCASE                              
*                                                                               
         MVC   CTEVKUSC,SVPTR      ADD PARTNER TO KEY                           
*                                                                               
         XC    KEY,KEY             MAKE SURE PARTNER CODE ON FILE               
         LA    R4,KEY                                                           
         USING CTEPKEY,R4          ESTABLISH PARTNER KEY                        
*                                                                               
         MVI   CTEPKTYP,CTEPKTYQ   SET RECORD TYPE                              
         MVI   CTEPKSTY,CTEPKSTQ   SET RECORD SUB-TYPE                          
*                                                                               
         MVC   CTEPKREP,SVPTR      SET PARTNER CODE IN KEY                      
*                                                                               
         MVC   AIO,AIO2            USE IOA2                                     
*                                                                               
         GOTOR HIGH                READ FOR RECORD                              
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         CLC   CTEPKEY,KEYSAVE     MUST FIND RECORD                             
         BNE   VKPTRNF                                                          
*                                                                               
         MVC   KEY,SVCTEVKY        SET STARTING KEY                             
         LA    R4,KEY              SWITCH KEY POINTER                           
*                                                                               
         USING CTEVKEY,R4          ESTABLISH VENDOR KEY                         
*                                                                               
         MVI   CTEVKARS,CTEVKRPQ   SET FOR REP TYPE RECORD                      
*                                                                               
         GOTOR HIGH                READ FOR RECORD ON FILE                      
*                                                                               
         CLC   CTEVKEY,KEYSAVE     CHECK IF VDR/PTR FOUND                       
         BE    VKPTRFD                                                          
*                                  VNDR/PTR NOT FOUND                           
         CLI   ACTNUM,ACTADD       IF ADDING NEW VDR/PTR                        
         BNE   VKNTFND                                                          
*                                                                               
         CLI   SCROFCH+5,0            OFFICE MUST NOT BE ENTERED                
         BNE   VKNOOFC                                                          
*                                                                               
         B     VKPTR90            ELSE OKAY                                     
*                                                                               
VKPTRFD  DS    0H                  RECORD FOUND ON FILE                         
*                                                                               
VKPTR90  DS    0H                                                               
*                                                                               
         MVI   SVARS,CTEVKRPQ      SET RECORD TYPE TO REP                       
*                                                                               
         LA    R4,SVCTEVKY         SWITCH KEY POINTER                           
*                                                                               
VKPTRX   DS    0H                                                               
*                                                                               
*        VALIDATE STATION CODE                                                  
*                                                                               
VKSTA    DS    0H                                                               
*                                                                               
         LA    R4,SVCTEVKY         POINT TO SAVED KEY                           
*                                                                               
         XC    SVSTA,SVSTA         INIT PARTNER CODE SAVEAREA                   
*                                                                               
         LA    R2,SCRSTAH          POINT TO PARTNER CODE FIELD                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          OKAY IF NO ENTRY                             
         BZ    VKSTAX                                                           
*                                                                               
         CLI   SVARS,0             ERROR IF RECORD TYPE SET                     
         BNE   VKARSER                                                          
*                                                                               
         CHI   RF,L'CTEVKUID       MUST BE LESS THAN MAX LENGTH                 
         BH    VKLARGE                                                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVSTA(0),8(R2)      SAVE STATION CODE                            
*                                                                               
         OC    SVSTA,SPACES        FORCE UPPERCASE                              
*                                                                               
         MVC   CTEVKUID,SVSTA      ADD USERID TO KEY                            
*                                                                               
         XC    KEY,KEY             MAKE SURE USERID ON FILE                     
         LA    R4,KEY                                                           
         USING CTIKEY,R4           ESTABLISH IDI KEY                            
*                                                                               
         MVI   CTIKTYP,CTIKTYPQ    SET RECORD TYPE                              
*                                                                               
         MVC   CTIKID,SVSTA        SET USERID CODE IN KEY                       
*                                                                               
         MVC   AIO,AIO2            USE IOA2                                     
*                                  READ FOR IDI RECORD                          
         MVC   KEYSAVE,KEY         SAVE KEY                                     
         GOTOR DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO                       
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         CLC   CTIKEY,KEYSAVE      MUST FIND RECORD                             
         BNE   VKUIDNF                                                          
*                                                                               
         CLI   SCROFCH+5,0            OFFICE MUST NOT BE ENTERED                
         BNE   VKNOOFC                                                          
*                                                                               
         B     VKSTA90            ELSE OKAY                                     
*                                                                               
         DROP  R4                                                               
*                                                                               
VKSTAFD  DS    0H                  RECORD FOUND ON FILE                         
*                                                                               
VKSTA90  DS    0H                                                               
*                                                                               
         MVI   SVARS,CTEVKLSQ      SET RECORD TYPE                              
*                                                                               
         LA    R4,SVCTEVKY         SWITCH KEY POINTER                           
*                                                                               
VKSTAX   DS    0H                                                               
*                                                                               
*        VALIDATE ROUTING AGENCY CODE                                           
*                                                                               
VKAGY    DS    0H                                                               
*                                                                               
         LA    R4,SVCTEVKY         POINT TO SAVED KEY                           
         USING CTEVKEY,R4          ESTABLISH VENDOR KEY                         
*                                                                               
         XC    SVAGY,SVAGY         INIT PARTNER CODE SAVEAREA                   
*                                                                               
         LA    R2,SCRAGYH          POINT TO PARTNER CODE FIELD                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          OKAY IF NO ENTRY                             
         BZ    VKAGYX                                                           
*                                                                               
         CLI   SVARS,0             ERROR IF RECORD TYPE SET                     
         BNE   VKARSER                                                          
*                                                                               
         CHI   RF,L'CTEVKRAG       MUST BE LESS THAN MAX LENGTH                 
         BH    VKLARGE                                                          
*                                                                               
         CHI   RF,2                MUST BE AT LEAST 2 CHARACTERS                
         BL    VKLESSER                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVAGY(0),8(R2)      SAVE AGENCY CODE                             
*                                                                               
         OC    SVAGY,SPACES        FORCE UPPERCASE                              
*                                                                               
         MVC   CTEVKRAG,SVAGY      ADD AGENCY TO KEY                            
*                                                                               
         MVC   KEY,SVCTEVKY        SET STARTING KEY                             
         LA    R4,KEY              SWITCH KEY POINTER                           
*                                                                               
         USING CTEVKEY,R4          ESTABLISH VENDOR KEY                         
*                                                                               
         MVI   CTEVKARS,CTEVKAGQ   SET AGENCY RECORD TYPE                       
*                                                                               
         GOTOR HIGH                READ FOR RECORD ON FILE                      
*                                                                               
         CLC   CTEVKEY,KEYSAVE     CHECK IF VDR/AGY FOUND                       
         BE    VKAGYFD                                                          
*                                  VNDR/AGY NOT FOUND                           
         CLI   ACTNUM,ACTADD       IF ADDING NEW VDR/AGY                        
         BNE   VKAGYNF                                                          
*                                                                               
         CLI   SCROFCH+5,0            OFFICE MUST NOT BE ENTERED                
         BNE   VKNOOFC                                                          
*                                                                               
         B     VKAGY90            ELSE OKAY                                     
*                                                                               
VKAGYFD  DS    0H                  RECORD FOUND ON FILE                         
*                                                                               
VKAGY90  DS    0H                                                               
*                                                                               
         MVI   SVARS,CTEVKAGQ      SET RECORD TYPE TO REP                       
*                                                                               
         LA    R4,SVCTEVKY         SWITCH KEY POINTER                           
*                                                                               
VKAGYX   DS    0H                                                               
*                                                                               
*        VALIDATE OFFICE CODE                                                   
*                                                                               
VKOFC    DS    0H                                                               
*                                                                               
         XC    SVOFC,SVOFC         INIT OFFICE CODE SAVEAREA                    
*                                                                               
         LA    R2,SCROFCH          POINT TO OFFICE CODE FIELD                   
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          OKAY IF NO ENTRY                             
         BZ    VKOFCX                                                           
*                                                                               
         CLI   SVARS,CTEVKRPQ      OFFICE ONLY WITH PTR                         
         BE    *+8                                                              
         CLI   SVARS,CTEVKAGQ      OR AGENCY RECORDS                            
         BNE   VKNOOFC                                                          
*                                                                               
         CHI   RF,L'CTEVKOFF       MUST BE OF LENGTH 2                          
         BNE   VKNINV                                                           
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVOFC(0),8(R2)      SAVE OFFICE CODE                             
*                                                                               
         OC    SVOFC,SPACES        FORCE UPPERCASE                              
*                                                                               
         CLI   SVARS,CTEVKRPQ      IF PARTNER RECORD                            
         BNE   *+10                                                             
         MVC   CTEVKOFF,SVOFC         ADD OFFICE TO KEY                         
*                                                                               
         CLI   SVARS,CTEVKAGQ      IF AGENCY  RECORD                            
         BNE   *+10                                                             
         MVC   CTEVKROF,SVOFC         ADD OFFICE TO KEY                         
*                                                                               
         MVC   KEY,SVCTEVKY        SET STARTING KEY                             
*                                                                               
VKVOFC90 DS    0H                                                               
*                                                                               
         LA    R4,SVCTEVKY         SWITCH KEY POINTER                           
*                                                                               
VKOFCX   DS    0H                                                               
*                                                                               
VK05     DS    0H                                                               
*                                                                               
VK90     DS    0H                                                               
*                                                                               
         MVC   CTEVKARS,SVARS      SET RECORD TYPE                              
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING                                    
         BNE   VK99                                                             
*                                                                               
         BRAS  RE,GETFTRS          DISPLAY FEATURES LIST                        
*                                                                               
         L     R4,AIO              POINT TO RECORD BUILD AREA                   
         MVC   CTEVKEY,SVCTEVKY    COPY KEY                                     
*                                                                               
         LA    R4,KEY              SWITCH POINTERS                              
         MVC   KEY,SVCTEVKY        SET STARTING KEY                             
*                                                                               
VK99     DS    0H                                                               
*                                                                               
         MVC   KEY,SVCTEVKY        SET KEY                                      
*                                                                               
VKX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
VKMISS   LA    RF,MISSING          REQUIRED FIELD                               
         B     VKERR                                                            
*                                                                               
VKLESSER LA    RF,TOOSMALL         INPUT TOO SMALL                              
         B     VKERR                                                            
*                                                                               
VKLARGE  LA    RF,TOOBIG           INPUT TOO LARGE                              
         B     VKERR                                                            
*                                                                               
VKNOFLD  LA    RF,NOKEYFLD         NO OTHER KEY FIELDS ALLOWED                  
         B     VKERR                                                            
*                                                                               
VKPTRNF  LA    RF,PTRNOTFD         PARTNER NOT ON FILE                          
         B     VKERR                                                            
*                                                                               
VKAGYNF  DS    0H                                                               
VKNTFND  LA    RF,NOTFOUND         RECORD NOT ON FILE                           
         B     VKERR                                                            
*                                                                               
VKNOOFC  LA    RF,OFCNOTOK         OFFICE ENTRY NOT ALLOWED                     
         B     VKERR                                                            
*                                                                               
VKARSER  LA    RF,ARSERR           ONLY ONE OF PTR/STA/AGY ALLOWED              
         B     VKERR                                                            
*                                                                               
VKUIDNF  LA    RF,NOUSERID         USERID NOT ON FILE                           
         B     VKERR                                                            
*                                                                               
VKNINV   LA    RF,INVALID          INVALID INPUT                                
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
         MVI   GTMSYS,X'08'        REP MESSAGE SYSTEM                           
         STCM  RF,3,GTMSGNO        SET ERROR CODE                               
*                                                                               
         GOTOR ERREX                                                            
*                                                                               
TOOSMALL EQU   1029                INPUT TOO SMALL                              
NOKEYFLD EQU   1030                NO OTHER KEY FIELDS ALLOWED                  
TOOBIG   EQU   1031                INPUT TOO BIG                                
PTRNOTFD EQU   1032                PARTNER NOT FOUND                            
OFCNOTOK EQU   1033                OFFICE NOT ALLOWED                           
ARSERR   EQU   1034                ONLY ONE OF PTR/STA/AGY                      
NOUSERID EQU   1035                USERID NOT ON FILE                           
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,RE                                                            
*                                                                               
         TITLE 'TA0A59  DARE VENDOR RECORD - DK'                                
***********************************************************************         
*                                                                     *         
*     DISPLAY  KEY ROUTINE                                            *         
*                                                                     *         
***********************************************************************         
*                                                                               
DK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AIO                                                           
         USING CTEVKEY,R4          ESTABLISH VENDOR RECORD KEY                  
*                                                                               
         MVC   SVCTEVKY,CTEVKEY    SAVE KEY                                     
*                                                                               
*                                  DISPLAY VENDOR CODE                          
         LA    RF,L'CTEVKVND       LENGTH OF VENDOR CODE                        
         FOUT  SCRVNDRH,CTEVKVND,(RF)                                           
*                                                                               
*        INIT PTR/STA/AGY FIELDS                                                
*                                                                               
         LA    RF,L'CTEVKUSC       LENGTH OF PARTNER ID                         
         FOUT  SCRPTRH,SPACES,(RF)  CLEAR   PARTNER ID                          
*                                                                               
         LA    RF,L'CTEVKRAG       LENGTH OF AGENCY  ID                         
         FOUT  SCRPTRH,SPACES,(RF)  CLEAR   AGENCY  ID                          
*                                                                               
         LA    RF,L'CTEVKUID       LENGTH OF STATION ID                         
         FOUT  SCRPTRH,SPACES,(RF)  CLEAR   STATION ID                          
*                                                                               
         LA    RF,L'CTEVKOFF       LENGTH OF OFFICE ID                          
         FOUT  SCROFCH,CTEVKOFF,(RF)  DISPLAY OFFICE ID                         
*                                                                               
*        DISPLAY PARTNER CODE                                                   
*                                                                               
         CLI   CTEVKARS,CTEVKRPQ   IF PARTNER TYPE                              
         BNE   DKPTRX                                                           
*                                                                               
         LA    RF,L'CTEVKUSC       LENGTH OF PARTNER CODE                       
         FOUT  SCRPTRH,CTEVKUSC,(RF)                                            
*                                                                               
         OC    CTEVKOFF,CTEVKOFF   IF OFFICE PRESENT                            
         BZ    DKPTRX                                                           
*                                     DISPLAY OFFICE CODE                       
         LA    RF,L'CTEVKOFF          LENGTH OF OFFICE CODE                     
         FOUT  SCROFCH,CTEVKOFF,(RF)                                            
*                                                                               
         B     DKARSX                                                           
*                                                                               
DKPTRX   DS    0H                                                               
*                                                                               
*        DISPLAY LOCAL STATION                                                  
*                                                                               
         CLI   CTEVKARS,CTEVKLSQ   IF LOCAL STATION TYPE                        
         BNE   DKSTAX                                                           
*                                                                               
         LA    RF,L'CTEVKUID       LENGTH OF STATION CODE                       
         FOUT  SCRSTAH,CTEVKUID,(RF)                                            
*                                                                               
         B     DKARSX                                                           
*                                                                               
DKSTAX   DS    0H                                                               
*                                                                               
*        DISPLAY AGENCY CODE                                                    
*                                                                               
         CLI   CTEVKARS,CTEVKAGQ   IF AGENCY  TYPE                              
         BNE   DKAGYX                                                           
*                                                                               
         LA    RF,L'CTEVKRAG       LENGTH OF AGENCY  CODE                       
         FOUT  SCRAGYH,CTEVKRAG,(RF)                                            
*                                                                               
         OC    CTEVKROF,CTEVKROF   IF OFFICE PRESENT                            
         BZ    DKAGYX                                                           
*                                     DISPLAY OFFICE CODE                       
         LA    RF,L'CTEVKROF          LENGTH OF OFFICE CODE                     
         FOUT  SCROFCH,CTEVKROF,(RF)                                            
*                                                                               
DKAGYX   DS    0H                                                               
*                                                                               
         B     DKARSX                                                           
*                                                                               
DKARSX   DS    0H                                                               
*                                                                               
DKX      DS    0H                                                               
         XIT1                      EXIT                                         
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'TA0A59  DARE VENDOR RECORD - VR'                                
***********************************************************************         
*                                                                     *         
*     VALIDATE RECORD ROUTINE                                         *         
*                                                                     *         
***********************************************************************         
*                                                                               
VR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVCTEVKY,KEY        SAVE THE RECORD KEY                          
*                                                                               
         BRAS  RE,GETFTRS          FILL IN FEATURES TABLE                       
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R4,AIO                                                           
         USING CTEVRECD,R4         ESTABLISH FOUND RECORD                       
*                                                                               
*        REMOVE ALL FEATURE ELEMENTS FOR FEATURES IN TABLE                      
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF ADDING RECORD                        
         BE    VRDELX                                                           
*                                                                               
         MVI   ELCODE,CTEVRELQ     SET FOR FEATURE ELEMENTS                     
         LR    R6,R4                                                            
         BRAS  RE,GETEL            FIND FIRST FEATURE ELEM                      
*                                                                               
VRDELLP  DS    0H                                                               
*                                                                               
         BNZ   VRDELDN             END OF RECORD                                
*                                                                               
         USING CTEVRD,R6           ESTABLISH FEATURES ELEMENT                   
*                                                                               
         LA    R3,FTRTAB           POINT TO FEATURES TABLE                      
         USING FTRTABD,R3          ESTABLSIH TABLE                              
*                                                                               
*        MATCH FEATURE TO TABLE ENTRY                                           
*                                                                               
VRDFTRLP DS    0H                                                               
*                                                                               
         OC    FTRTNUM,FTRTNUM     DONE AT END OF TABLE                         
         BZ    VRDFTRDN                                                         
*                                                                               
         CLC   CTEVFEAT,FTRTNUM    FIND FEATURE IN TABLE                        
         BE    VRDFTRFD                                                         
*                                                                               
VRDFTRCN DS    0H                                                               
*                                                                               
         LA    R3,FTRTLENQ(R3)     BUMP TO NEXT FEATURE IN TABLE                
*                                                                               
         B     VRDFTRLP                                                         
*                                                                               
VRDFTRFD DS    0H                                                               
*                                                                               
         MVI   CTEVREL,X'FF'       FLAG ELEMENT FOR REMOVAL                     
*                                                                               
VRDFTRDN DS    0H                                                               
*                                                                               
VRDELCN  DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           NEXT FEATURES ELEMENT                        
*                                                                               
         B     VRDELLP                                                          
*                                                                               
VRDELDN  DS    0H                                                               
*                                                                               
*        DELETE ELEMENTS FLAGGED FOR DELETION                                   
*                                                                               
         MVI   ELCODE,X'FF'        DELETE INDICATOR                             
*                                                                               
VRREMLP  DS    0H                                                               
*                                                                               
         GOTOR REMELEM             REMOVE ELEMENT                               
*                                                                               
         OC    ELEMENT,ELEMENT     DONE IF NO ELEMENT REMOVED                   
         BZ    VRREMDN                                                          
*                                                                               
VRREMCN  DS    0H                                                               
*                                                                               
         B     VRREMLP                                                          
*                                                                               
VRREMDN  DS    0H                                                               
*                                                                               
*        NOTE R6 ==> END OF RECORD                                              
*                                                                               
         B     VRADDX                                                           
*                                                                               
VRDELX   DS    0H                                                               
*                                                                               
*        ADD NEW RECORD TO THE FILE                                             
*                                                                               
         MVC   KEY,SVCTEVKY        SET CURRENT KEY                              
*                                                                               
         XC    CTEVRECD(100),CTEVRECD   INIT RECORD BUILD AREA                  
         MVC   CTEVKEY,KEY         SET KEY                                      
*                                                                               
         LA    RF,CTEVDAT-CTEVKEY  MINIMUM LENGTH OF RECORD                     
*                                                                               
         STCM  RF,3,CTEVFLEN       SET MINIMUM RECORD LENGTH                    
*                                                                               
VRADDX   DS    0H                                                               
*                                                                               
*        ADD FEATURE ELEMENTS TO RECORD                                         
*                                                                               
         LA    R6,ELEMENT          POINT TO ELEMENT BUILD AREA                  
         XC    ELEMENT,ELEMENT                                                  
*                                                                               
         LA    R2,SCRSL01H         POINT TO FIRST FEATURE SELECTION             
         USING FLLINED,R2                                                       
         LA    R3,FTRTAB           POINT TO FEATURES TABLE                      
         LHI   R0,PAGE#Q           NUMBER OF ELEMENTS IN COLUMN                 
         MVI   COLSW,1             FIRST COLUMN                                 
*                                                                               
VRFTRLP  DS    0H                                                               
*                                                                               
         USING CTEVRD,R6           ESTABLISH FEATURES ELEMENT                   
*                                                                               
         XC    0(CTEVRLNQ,R6),0(R6) INIT ELEMENT                                
*                                                                               
         USING FTRTABD,R3          ESTABLISH FEATURES TABLE                     
*                                                                               
         OC    FTRTNUM,FTRTNUM     DONE AT END OF TABLE                         
         BZ    VRFTRDN                                                          
*                                                                               
         TM    FLSL01H+1,X'20'     SKIP IF PROTECTED                            
         BO    VRFTRCN             NOT AT OUR LEVEL                             
*                                                                               
         CLI   FLSL01H+5,0         ANY INPUT?                                   
         BNE   VRFTR10             YES                                          
         OC    FLCD01,FLCD01       NO, DID WE HAVE SOMETHING BEFORE?            
         BZ    VRFTRCN                 NO, NOTHING TO WORRY ABOUT               
         B     VRFTR20                 YES, WE WANT THEM TO REMAIN              
*                                                                               
VRFTR10  OI    FLSL01H+6,X'80'                                                  
         OI    FLCD01H+6,X'80'                                                  
         OI    FLFT01H+6,X'80'                                                  
*                                                                               
         CLI   FLSL01,C'Y'         ONLY 'Y' AND 'N' ARE VALID                   
         BE    VRFTR20                                                          
         CLI   FLSL01,C'N'                                                      
         BNE   VRINV                                                            
         MVI   FLSL01,C' '                                                      
         XC    FLCD01,FLCD01                                                    
         OI    FLFT01H+1,X'08'     HIGH INTENSITY                               
         B     VRFTRCN                                                          
*                                                                               
VRFTR20  NI    FLFT01H+1,X'FF'-X'0C' NORMAL INTENSITY                           
         MVI   FLSL01,C' '                                                      
*                                                                               
         MVI   CTEVREL,CTEVRELQ    SET ELM ID                                   
         MVI   CTEVRLEN,CTEVRLNQ   SET ELM LENGTH                               
*                                                                               
         MVI   SVLVL,FTRTLVVQ      SET TO VENDOR LEVEL                          
         CLI   CTEVKARS,CTEVKRPQ   IF PARTNER RECORD                            
         BNE   VRFTRPRN                                                         
*                                                                               
         OC    CTEVKUSC,CTEVKUSC   IF PARTNER LEVEL                             
         BZ    *+8                                                              
         MVI   SVLVL,FTRTLVPQ      SET TO PARTNER LEVEL                         
*                                                                               
         OC    CTEVKOFF,CTEVKOFF                                                
         BZ    *+8                                                              
         MVI   SVLVL,FTRTLVOQ      SET TO OFFICE LEVEL                          
*                                                                               
         B     VRFTRLVX                                                         
*                                                                               
VRFTRPRN DS    0H                                                               
*                                                                               
         CLI   CTEVKARS,CTEVKLSQ   IF STATION RECORD                            
         BNE   VRFTRSTN                                                         
*                                                                               
         OC    CTEVKUID,CTEVKUID   IF STATION LEVEL                             
         BZ    *+8                                                              
         MVI   SVLVL,FTRTLVSQ         SET TO STATION LEVEL                      
*                                                                               
         B     VRFTRLVX                                                         
*                                                                               
VRFTRSTN DS    0H                                                               
*                                                                               
         CLI   CTEVKARS,CTEVKAGQ   IF AGENCY  RECORD                            
         BNE   VRFTAGYN                                                         
*                                                                               
         OC    CTEVKRAG,CTEVKRAG   IF AGENCY  LEVEL                             
         BZ    *+8                                                              
         MVI   SVLVL,FTRTLVAQ         SET TO AGENCY  LEVEL                      
*                                                                               
         OC    CTEVKROF,CTEVKROF                                                
         BZ    *+8                                                              
         MVI   SVLVL,FTRTLVRQ      SET TO OFFICE LEVEL                          
*                                                                               
VRFTAGYN DS    0H                                                               
*                                                                               
VRFTRLVX DS    0H                                                               
*                                                                               
*        VALIDATE FEATURE SELECTIONS                                            
*                                                                               
         OC    FTRTLVL,FTRTLVL     SKIP IF NO LEVEL                             
         BZ    *+14                                                             
         CLC   SVLVL,FTRTLVL       SKIP IF LOWER LEVEL                          
         BH    VRFTRCN                                                          
*                                                                               
         MVC   CTEVFEAT,FTRTNUM    SAVE FEATURE NUMBER                          
*                                                                               
         LA    R5,FLCD01H                                                       
         FOUT  (R5),SPACES,3       CLEAR CODE FIELD                             
*                                                                               
         CLI   SVLVL,FTRTLVVQ      SET LEVEL                                    
         BNE   *+10                                                             
         MVC   FLCD01,=C'VND'      VENDOR LEVEL                                 
*                                                                               
         CLI   SVLVL,FTRTLVPQ      SET LEVEL                                    
         BNE   *+10                                                             
         MVC   FLCD01,=C'PTR'      VENDOR LEVEL                                 
*                                                                               
         CLI   SVLVL,FTRTLVOQ      SET LEVEL                                    
         BNE   *+10                                                             
         MVC   FLCD01,=C'OFC'      VENDOR LEVEL                                 
*                                                                               
         CLI   SVLVL,FTRTLVSQ      SET LEVEL                                    
         BNE   *+10                                                             
         MVC   FLCD01,=C'STA'      STATION LEVEL                                
*                                                                               
         CLI   SVLVL,FTRTLVAQ      SET LEVEL                                    
         BNE   *+10                                                             
         MVC   FLCD01,=C'AGY'      AGENCY LEVEL                                 
*                                                                               
         CLI   SVLVL,FTRTLVRQ      SET LEVEL                                    
         BNE   *+10                                                             
         MVC   FLCD01,=C'ROF'      OFFICE LEVEL                                 
*                                                                               
         GOTOR ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
VRFTRCN  DS    0H                                                               
*                                                                               
         LA    R2,FLSL02H-FLSL01H(R2)    BUMP TO NEXT FIELD                     
         LA    R3,FTRTLENQ(R3)     BUMP TO NEXT TABLE ENTRY                     
*                                                                               
         BCT   R0,VRFTRLP                                                       
*                                                                               
         CLI   COLSW,2             DONE AT END OF SECOND COLUMN                 
         BE    VRFTRDN                                                          
*                                                                               
         LA    R2,SCRSL21H         START OF SECOND COLUMN                       
         MVI   COLSW,2             SECOND COLUMN                                
         LHI   R0,PAGE#Q           RESET ROW COUNTER                            
*                                                                               
         B     VRFTRLP                                                          
*                                                                               
VRFTRDN  DS    0H                                                               
*                                                                               
*****    MVI   0(R6),0             FORCE TRAILING NULLS                         
*                                                                               
*        IF ADDING/CHANGING VENDOR, VALIDATE VENDOR NAME                        
*                                                                               
         OC    CTEVKUSC,CTEVKUSC   SKIP IF PARTNER CODE PRESENT                 
         BNZ   VRVNDNMX                                                         
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELM BUILD AREA                          
         LA    R6,ELEMENT                                                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,SCRVNNMH+5                                                  
         BZ    VRVNDNMX            SKIP IF NO VENDOR NAME                       
*                                                                               
         USING CTVDND,R6           ESTABLISH NAME ELEMENT                       
*                                                                               
         MVI   CTVDNEL,CTVDNELQ    SET VENDOR NAME ELM CODE                     
         LA    R1,2(RF)            LENGTH OF ELEMENT                            
         STC   R1,CTVDNLEN         SET ELEMENT LENGTH                           
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CTVDN(0),SCRVNNM    SAVE VENDOR NAME                             
*                                                                               
         GOTOR ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
*****    LA    R6,CTVDN+1(RF)      BUMP TO NEXT ELEMENT                         
*                                                                               
VRVNDNMX DS    0H                                                               
*                                                                               
****     LR    RF,R6                                                            
****     SR    RF,R4               RECORD LENGTH                                
****     STCM  RF,3,CTEVFLEN       SET RECORD LENGTH                            
*                                                                               
         MVC   KEY,SVCTEVKY        RESTORE KEY                                  
*                                                                               
VRX      DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
VRINV    MVI   ERROR,INVALID       INVALID FIELD                                
         B     VRERR                                                            
*                                                                               
VRMISS   MVI   ERROR,MISSING       REQUIRED FIELD                               
         B     VRERR                                                            
*                                                                               
VRERR    DS    0H                                                               
         GOTOR ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
         TITLE 'TA0A59  DARE VENDOR RECORD - DR'                                
***********************************************************************         
*                                                                     *         
*     DISPLAY  RECORD ROUTINE                                         *         
*                                                                     *         
***********************************************************************         
*                                                                               
DR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,PFK                                                           
*                                                                               
*        VALIDATE PAGE NUMBER                                                   
*                                                                               
DRPAGE   DS    0H                                                               
*                                                                               
         CLI   SCRPAGEH+5,0        IF NO PAGE ENTERED                           
         BNZ   *+14                                                             
         ZAP   SVPAGE,=P'1'           DEFAULT TO ONE                            
         B     DRPAGEX                                                          
*                                                                               
         LLC   RF,SCRPAGEH+5       GET INPUT LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SCRPAGE(0)      PACK PAGE NUMBER                             
*                                                                               
         TP    DUB                 IF NOT A NUMBER                              
         BZ    *+14                                                             
         ZAP   SVPAGE,=P'1'           DEFAULT TO ONE                            
         B     DRPAGEX                                                          
*                                                                               
         ZAP   SVPAGE,DUB          SAVE ENTERED PAGE NUMBER                     
*                                                                               
DRPAGEX  DS    0H                                                               
*                                                                               
         FOUT  SCRPAGEH,SPACES,3   INIT PAGE FIELD                              
*                                                                               
         EDIT  (P8,SVPAGE),SCRPAGE,0,ALIGN=LEFT                                 
*                                                                               
         STC   R0,SCRPAGEH+5       SET FIELD LENGTH                             
*                                                                               
         L     R4,AIO                                                           
         USING CTEVRECD,R4         ESTABLISH VENDOR RECORD                      
*                                                                               
         LA    RF,L'CTEVKVND       LENGTH OF VENDOR ID                          
         FOUT  SCRVNDRH,CTEVKVND,(RF)  DISPLAY VENDOR ID                        
*                                                                               
*        DISPLAY KEY FIELDS                                                     
*                                                                               
DRPTR    DS    0H                                                               
*                                                                               
         CLI   CTEVKARS,CTEVKRPQ   IF PARTNER RECORD                            
         BNE   DRPTRN                                                           
*                                                                               
         LA    RF,L'CTEVKUSC       LENGTH OF PARTNER ID                         
         FOUT  SCRPTRH,CTEVKUSC,(RF)  DISPLAY PARTNER ID                        
*                                                                               
         OC    CTEVKOFF,CTEVKOFF   SKIP IF NO OFFICE CODE                       
         BZ    DRPTRX                                                           
*                                                                               
         LA    RF,L'CTEVKOFF       LENGTH OF OFFICE ID                          
         FOUT  SCROFCH,CTEVKOFF,(RF)  DISPLAY OFFICE ID                         
*                                                                               
DRPTRX   DS    0H                                                               
*                                                                               
         B     DRKEYX                                                           
*                                                                               
DRPTRN   DS    0H                                                               
*                                                                               
DRAGY    DS    0H                                                               
*                                                                               
         CLI   CTEVKARS,CTEVKAGQ   IF AGENCY  RECORD                            
         BNE   DRAGYN                                                           
*                                                                               
         LA    RF,L'CTEVKRAG       LENGTH OF AGENCY  ID                         
         FOUT  SCRAGYH,CTEVKRAG,(RF)  DISPLAY AGENCY  ID                        
*                                                                               
         OC    CTEVKROF,CTEVKROF   SKIP IF NO OFFICE CODE                       
         BZ    DRAGYX                                                           
*                                                                               
         LA    RF,L'CTEVKROF       LENGTH OF OFFICE ID                          
         FOUT  SCROFCH,CTEVKROF,(RF)  DISPLAY OFFICE ID                         
*                                                                               
DRAGYX   DS    0H                                                               
*                                                                               
         B     DRKEYX                                                           
*                                                                               
DRAGYN   DS    0H                                                               
*                                                                               
DRSTA    DS    0H                                                               
*                                                                               
         CLI   CTEVKARS,CTEVKLSQ   IF STATION RECORD                            
         BNE   DRSTAN                                                           
*                                                                               
         LA    RF,L'CTEVKUID          LENGTH OF USER    ID                      
         FOUT  SCRSTAH,CTEVKUID,(RF)  DISPLAY USER    ID                        
*                                                                               
DRSTAX   DS    0H                                                               
*                                                                               
         B     DRKEYX                                                           
*                                                                               
DRSTAN   DS    0H                                                               
*                                                                               
DRKEYX   DS    0H                                                               
*                                                                               
         BRAS  RE,GETFTRS          BUILD FEATURES TABLE                         
*                                                                               
*        DETERMINE LEVEL OF RECORD IN CORE                                      
*                                                                               
DRLVL    DS    0H                                                               
*                                                                               
         MVI   SVLVL,FTRTLVVQ      SET TO VENDOR LEVEL                          
*                                                                               
         CLI   CTEVKARS,CTEVKRPQ   IF PARTNER RECORD                            
         BNE   DRLVPRN                                                          
*                                                                               
         OC    CTEVKUSC,CTEVKUSC      DONE IF NO PARTNER                        
         BZ    DRLVPRX                                                          
*                                                                               
         MVI   SVLVL,FTRTLVPQ         SET TO PARTNER LEVEL                      
*                                                                               
         OC    CTEVKOFF,CTEVKOFF      DONE IF NO OFFICE                         
         BZ    DRLVOFX                                                          
*                                                                               
         MVI   SVLVL,FTRTLVOQ      SET OF OFFICE LEVEL                          
*                                                                               
DRLVOFX  DS    0H                                                               
*                                                                               
DRLVPRX  DS    0H                                                               
*                                                                               
         B     DRLVLX                                                           
*                                                                               
DRLVPRN  DS    0H                                                               
*                                                                               
         CLI   CTEVKARS,CTEVKLSQ   IF STATION RECORD                            
         BNE   DRLVSTN                                                          
*                                                                               
         OC    CTEVKUID,CTEVKUID      DONE IF NO STATION                        
         BZ    DRLVSTX                                                          
*                                                                               
         MVI   SVLVL,FTRTLVSQ         SET TO STATION LEVEL                      
*                                                                               
DRLVSTX  DS    0H                                                               
*                                                                               
         B     DRLVLX                                                           
*                                                                               
DRLVSTN  DS    0H                                                               
*                                                                               
         CLI   CTEVKARS,CTEVKAGQ   IF AGENCY  RECORD                            
         BNE   DRLVAGN                                                          
*                                                                               
         OC    CTEVKRAG,CTEVKRAG      DONE IF NO AGENCY                         
         BZ    DRLVAGX                                                          
*                                                                               
         MVI   SVLVL,FTRTLVAQ         SET TO SGENCY  LEVEL                      
*                                                                               
         OC    CTEVKROF,CTEVKROF      DONE IF NO OFFICE                         
         BZ    DRLVROX                                                          
*                                                                               
         MVI   SVLVL,FTRTLVRQ         SET OFFICE LEVEL                          
*                                                                               
DRLVROX  DS    0H                                                               
*                                                                               
DRLVAGX  DS    0H                                                               
*                                                                               
         B     DRLVLX                                                           
*                                                                               
DRLVAGN  DS    0H                                                               
*                                                                               
DRLVLX   DS    0H                                                               
*                                                                               
*        DISPLAY VENDOR NAME IF PRESENT                                         
*                                                                               
         LA    R2,SCRVNNMH         VENDOR NAME FIELD                            
         FOUT  (R2),SPACES,(RF)    INIT FIELD                                   
*                                                                               
         CLI   SVLVL,FTRTLVVQ      IF NOT A VENDOR RECORD                       
         BE    *+8                                                              
         OI    6(R2),X'20'            PROTECT                                   
*                                                                               
         OC    SVVNDNM,SVVNDNM     SKIP IF NO VENDOR NAME                       
         BZ    DRVNDNMX                                                         
*                                                                               
         MVC   SCRVNNM,SVVNDNM     DISPLAY VENDOR NAME                          
*                                                                               
DRVNDNMX DS    0H                                                               
*                                                                               
*        DISPLAY FEATURES                                                       
*                                                                               
         LA    R3,FTRTAB           POINT TO FEATURE TABLE                       
         LA    R2,SCRSL01H         POINT TO FIRST FEATURE FLD                   
         USING FLLINED,R2                                                       
         LHI   R0,PAGE#Q           ENTRY COUNTER                                
         MVI   COLSW,1             SET COLUMN SWITCH TO 1                       
*                                                                               
DRTABLP  DS    0H                                                               
         USING FTRTABD,R3          ESTABLISH FILTER TABLE                       
         OC    FTRTNUM,FTRTNUM     DONE AT END OF TABLE                         
         BZ    DRTABDN                                                          
*                                                                               
         LA    RF,L'FLCD01         LENGTH OF CODE FIELD                         
         LA    R6,FLCD01H-FLSL01H(R2)  FOR FOUT                                 
         FOUT  (R6),SPACES,(RF)    CLEAR FIELD                                  
*                                                                               
         NI    FLSL01H+1,X'FF'-X'20'   TURN OFF PROTECTION                      
         OI    FLSL01H+1,X'08'         HIGH INTENSITY                           
         OI    FLSL01H+6,X'80'                                                  
         OI    FLCD01H-FLSL01H+1(R2),X'08'                                      
         OI    FLFT01H-FLSL01H+1(R2),X'08'                                      
         OC    FTRTLVL,FTRTLVL     SKIP IF NO LEVEL FOR FEATURE                 
         BZ    DRTAB10                                                          
         NI    FLFT01H-FLSL01H+1(R2),X'FF'-X'0C'                                
         CLC   SVLVL,FTRTLVL       IF AT LOWER LEVEL                            
         BNH   DRTAB10                                                          
         OI    1(R2),X'20'         PROTECT FIELD                                
         NI    1(R2),X'FF'-X'0C'   NORMAL INTENSITY                             
         NI    FLCD01H-FLSL01H+1(R2),X'FF'-X'0C'                                
*                                                                               
DRTAB10  CLI   FTRTLVL,FTRTLVVQ    IF VENDOR LEVEL                              
         BE    DRTABV                                                           
         CLI   FTRTLVL,FTRTLVPQ    IF PARTNER LEVEL                             
         BE    DRTABP                                                           
         CLI   FTRTLVL,FTRTLVOQ    IF OFFICE LEVEL                              
         BE    DRTABO                                                           
         CLI   FTRTLVL,FTRTLVSQ    IF STATION LEVEL                             
         BE    DRTABS                                                           
         CLI   FTRTLVL,FTRTLVAQ    IF AGENCY LEVEL                              
         BE    DRTABA                                                           
         CLI   FTRTLVL,FTRTLVRQ    IF ROUTING OFFICE LEVEL                      
         BNE   DRTAB20                                                          
*                                                                               
DRTABR   MVC   FLCD01-FLSL01H(L'FLCD01,R2),=C'ROF'  DISPLAY TAG                 
         J     DRTAB15                                                          
DRTABV   MVC   FLCD01-FLSL01H(L'FLCD01,R2),=C'VND'                              
         J     DRTAB15                                                          
DRTABP   MVC   FLCD01-FLSL01H(L'FLCD01,R2),=C'PTR'                              
         J     DRTAB15                                                          
DRTABO   MVC   FLCD01-FLSL01H(L'FLCD01,R2),=C'OFC'                              
         J     DRTAB15                                                          
DRTABS   MVC   FLCD01-FLSL01H(L'FLCD01,R2),=C'STA'                              
         J     DRTAB15                                                          
DRTABA   MVC   FLCD01-FLSL01H(L'FLCD01,R2),=C'AGY'                              
*                                                                               
DRTAB15  MVI   FLCD01H-FLSL01H+5(R2),3                                          
*                                                                               
*        DISPLAY FEATURE NAME                                                   
*                                                                               
DRTAB20  LA    RF,L'FTRTNAME       LENGTH OF DESCRIPTION                        
         LA    R6,FLFT01H-FLSL01H(R2)                                           
         FOUT  (R6),FTRTNAME,(RF)                                               
*                                                                               
DRTABCN  DS    0H                                                               
*                                                                               
         LA    R3,FTRTLENQ(R3)     NEXT TABLE ENTRY                             
         LA    R2,(FLSL02H-FLSL01H)(R2) NEXT SELECT IN COLUMN                   
*                                                                               
         BCT   R0,DRTABLP          NEXT ENTRY IN COLUMN                         
*                                                                               
         CLI   COLSW,2             DONE IF END OF SECOND COLUMN                 
         BE    DRSCRDN             SCREEN COMPLETELY FILLED                     
*                                                                               
         LA    R2,SCRSL21H         POINT TO START OF SECOND COLUMN              
         LHI   R0,PAGE#Q           RESET ENTRY COUNTER                          
         MVI   COLSW,2             SECOND COLUMN                                
         B     DRTABLP             FILL SECOND COLUMN                           
*                                                                               
DRTABDN  DS    0H                                                               
*                                                                               
*        CLEAR AND PROTECT REMAINING FIELDS ON SCREEN                           
*                                                                               
DRSCRLP  DS    0H                                                               
*                                                                               
         OI    FLSL01H+1,X'20'     PROTECT FIELD                                
         OI    FLSL01H+6,X'80'                                                  
         LA    RF,L'FLCD01         SELECT FIELD LENGTH                          
         LA    R6,FLCD01H-FLSL01H(R2)                                           
         FOUT  (R6),SPACES,(RF)    CLEAR FIELD                                  
*                                                                               
         LA    RF,L'FLFT01         FIELD LENGTH                                 
         LA    R6,FLFT01H-FLSL01H(R2)                                           
         FOUT  (R6),SPACES,(RF)                                                 
*                                                                               
DRSCRCN  DS    0H                                                               
*                                                                               
         LA    R2,(FLSL02H-FLSL01H)(R2) NEXT SELECT IN COLUMN                   
         BCT   R0,DRSCRLP                                                       
*                                                                               
         CLI   COLSW,2             END OF SCREEN                                
         BNL   DRSCRDN                                                          
*                                                                               
         LA    R0,PAGE#Q           LINES IN COLUMN                              
         LA    R2,SCRSL21H         START OF SECOND COLUMN                       
         MVI   COLSW,2             SECOND COLUMN                                
*                                                                               
         B     DRSCRLP                                                          
*                                                                               
DRSCRDN  DS    0H                                                               
*                                                                               
*        DISPLAY ACTIVITY                                                       
*                                                                               
         L     R6,AIO              POINT TO RECORD                              
         MVI   ELCODE,X'F1'        LOOK FOR ACTIVITY ELEMENT                    
         BRAS  RE,GETEL            FIND FIRST ELEMENT                           
         BNE   DRACTVX             NONE                                         
*                                                                               
         USING ACTVD,R6                                                         
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(11,LUPDATE)                            
*                                                                               
DRACTVX  XC    DMCB,DMCB                                                        
         GOTO1 GETTXT,DMCB,28,0,(C'I',0),(14,LUPDATE)                           
         OI    GENSTAT2,USMYOK                                                  
*                                                                               
DRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3,R4                                                            
*                                                                               
         TITLE 'TA0A59  DARE VENDOR RECORD - XR'                                
***********************************************************************         
*                                                                     *         
*     AFTER RECORD ADDED ROUTINE                                      *         
*                                                                     *         
***********************************************************************         
*                                                                               
XR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
XRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'TA0A59  DARE VENDOR RECORD - LR'                                
***********************************************************************         
*                                                                     *         
*     LIST RECORDS ROUTINE                                            *         
*                                                                     *         
***********************************************************************         
*                                                                               
LR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZAP   SVPAGE,=P'1'        FORCE PAGE 1 ON A SELECT                     
*                                                                               
         LA    R4,KEY                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR030               KEY IS LAST RECORD READ                      
*                                  SO GO CHECK VS. KEYSAVE                      
*                                                                               
         USING CTEVKEY,R4          ESTABLISH VENDOR KEY                         
*                                                                               
         MVI   CTEVKTYP,CTEVKTYQ   SET RECORD MAJOR TYPE                        
         MVI   CTEVKSTY,CTEVKSTQ   SET RECORD SECONDARY TYPE                    
*                                                                               
         MVC   CTEVKVND,SVVNDR     SET VENDOR CODE                              
*                                                                               
         OC    SVPTR,SVPTR         IF PARTNER ENTERED                           
         BZ    *+24                                                             
         MVI   CTEVKARS,CTEVKRPQ      SET FOR REP RECORDS                       
         MVC   CTEVKUSC,SVPTR         SET PARTNER CODE                          
         MVC   CTEVKOFF,SVOFC         SET OFFICE CODE                           
         B     LRARSX                                                           
*                                                                               
         OC    SVSTA,SVSTA         IF STATION ENTERD                            
         BZ    *+18                                                             
         MVI   CTEVKARS,CTEVKLSQ      SET FOR STA RECORDS                       
         MVC   CTEVKUID,SVSTA         SET STATION CODE                          
         B     LRARSX                                                           
*                                                                               
         OC    SVAGY,SVAGY         IF AGENCY  ENTERD                            
         BZ    *+14                                                             
         MVI   CTEVKARS,CTEVKAGQ      SET FOR AGY RECORDS                       
         MVC   CTEVKRAG,SVAGY         SET AGENCY  CODE                          
         MVC   CTEVKROF,SVOFC         SET OFFICE CODE                           
         B     LRARSX                                                           
*                                                                               
LRARSX   DS    0H                                                               
*                                                                               
LR010    GOTO1 HIGH                                                             
*                                                                               
         B     LR030                                                            
*                                                                               
LR020    GOTO1 HIGH                RE-POINT FILE                                
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
LR030    CLC   KEY(2),KEYSAVE      TEST FOR ALL DONE                            
         BNE   LR900                                                            
*                                                                               
         GOTO1 GETREC              READ IN FOUND RECORD                         
*                                                                               
         L     R4,AIO              POINT TO RECORD                              
*                                                                               
         LA    R2,LISTAR           ESTABLISH LIST                               
         USING LISTD,R2                                                         
*                                                                               
         MVC   LSLINE,SPACES       INIT LINE                                    
*                                                                               
         MVC   LSVND,CTEVKVND      DISPLAY VENDOR ID                            
*                                                                               
         CLI   CTEVKARS,CTEVKRPQ   IF REP RECORD TYPE                           
         BNE   *+16                                                             
         MVC   LSPTR,CTEVKUSC         DISPLAY PARTNER ID                        
         MVC   LSOFC,CTEVKOFF      DISPLAY OFFICE ID                            
*                                                                               
         CLI   CTEVKARS,CTEVKLSQ   IF USERID RECORD TYPE                        
         BNE   *+10                                                             
         MVC   LSSTA,CTEVKUID         DISPLAY STATION ID                        
*                                                                               
         CLI   CTEVKARS,CTEVKAGQ   IF ROUTING AGENCY RECORD TYPE                
         BNE   *+16                                                             
         MVC   LSAGY,CTEVKRAG         DISPLAY AGENCY  ID                        
         MVC   LSOFC,CTEVKROF      DISPLAY OFFICE ID                            
*                                                                               
LR080    GOTO1 LISTMON                                                          
         B     LR020                                                            
*                                                                               
LR900    DS    0H                                                               
*                                                                               
LRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'TA0A59  DARE VENDOR RECORD - PFK'                               
***********************************************************************         
*                                                                     *         
*        PF KEY HIT                                                   *         
*                                                                     *         
***********************************************************************         
*                                                                               
PFK      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   PHSCREEN,X'80'      DONE IF NOT DETAIL SCREEN                    
         BNE   PFKX                                                             
*                                                                               
         CLI   PFKEY,7             ONLY PF7 & PF8 RECOGNIZED                    
         BE    *+8                                                              
         CLI   PFKEY,8                                                          
         BNE   PFKX                                                             
*                                                                               
         TP    SVPAGE              IF PAGE # NOT KNOWN                          
         BZ    *+14                                                             
         ZAP   SVPAGE,=P'1'           DEFAULT TO PAGE 1                         
         B     PFKPGOUT               GO DISPLAY PAGE NUMBER                    
*                                                                               
         CLI   PFKEY,7             IF PAGE UP                                   
         BNE   *+14                                                             
         SP    SVPAGE,=P'1'           DECREMENT PAGE NUMBER                     
         B     PFKPGOUT                                                         
*                                                                               
         CLI   PFKEY,8             IF PAGE DOWN                                 
         BNE   PFKPGOUT                                                         
*                                                                               
         CLC   SCRFT21,SPACES      AND SECOND COLUMN HAS DATA                   
         BNH   *+14                                                             
         AP    SVPAGE,=P'1'           INCREMENT PAGE NUMBER                     
         B     PFKPGOUT                                                         
*                                                                               
         ZAP   SVPAGE,=P'1'        ELSE REVERT TO PAGE 1                        
*                                                                               
PFKPGOUT DS    0H                                                               
*                                                                               
         FOUT  SCRPAGEH,SPACES,3   INIT PAGE ON SCREEN                          
*                                                                               
*        DISPLAY NEW PAGE NUMBER                                                
*                                                                               
         EDIT  (P8,SVPAGE),SCRPAGE,0,ALIGN=LEFT                                 
*                                                                               
         STC   R0,SCRPAGEH+5       SET FIELD LENGTH                             
*                                                                               
         OI    GENSTAT2,RETEQSEL   RETURN TO THIS SCREEN                        
*                                                                               
PFKX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'TA0A59  DARE VENDOR RECORD - GETFTR'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD TABLE OF FEATURES                           *         
*                                                                     *         
*        TABLE CONSISTS OF FEATURE NUMBERS AND NAMES                  *         
*        AND SOURCE I.E. VENDOR, PTR OR OFFICE RECORD                 *         
*                                                                     *         
***********************************************************************         
*                                                                               
GETFTRS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AIO,AIO2            READ RECORDS INTO IOA2                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY              ESTABLISH FEATURES RECORD KEY                
         USING CTEFKEY,R5                                                       
*                                                                               
         MVI   CTEFKTYP,CTEFKTYQ   SET MAJOR RECORD TYPE                        
         MVI   CTEFKSTY,CTEFKSTQ   SET SUB-RECORD TYPE                          
*                                                                               
         LA    R3,FTRTAB           ESTABLISH FEATURE TABLE                      
         USING FTRTABD,R3                                                       
*                                                                               
         TP    SVPAGE              IF FIRST TIME                                
         BZ    *+10                                                             
         ZAP   SVPAGE,=P'1'           DEFAULT TO 1                              
*                                                                               
         CVB   RF,SVPAGE           CALCULATE # OF FIRST FEATURE IN TAB          
         BCTR  RF,0                DECREMENT FOR INDEXING                       
         MHI   RF,PAGE#Q                                                        
         LR    R0,RF               # OF FIRST FEATURE ON SCREEN                 
         AHI   R0,1                                                             
*                                                                               
         ZAP   FTRCTR,=P'0'        INIT TABLE ENTRIES COUNTER                   
*                                                                               
         XC    0(FTRTLENQ,R3),0(R3) INIT FIRST TABLE ENTRY                      
*                                                                               
         GOTOR HIGH                READ FIRST FEATURE RECORD                    
*                                                                               
GFTRLOOP DS    0H                                                               
*                                                                               
         CLC   KEY(2),KEYSAVE      DONE ON RECORD TYPE CHANGE                   
         BNE   GFTRDONE                                                         
*                                                                               
         GOTO1 GETREC              READ IN FOUND RECORD                         
*                                                                               
         L     R5,AIO              POINT TO FOUND RECORD                        
*                                                                               
         LA    R6,CTEFDAT          POINT TO FIRST ELEMENT                       
*                                                                               
GFTRNMLP DS    0H                                                               
*                                                                               
         USING CTFNMD,R6           ESTABLISH NAME ELEMENT                       
*                                                                               
         CLI   CTFNMEL,0           SKIP IF NONE FOUND                           
         BE    GFTRNMDN                                                         
*                                                                               
         CLI   CTFNMEL,CTFNMELQ    FIND NAME ELEMENT                            
         BE    GFTRNMFD            1ST ELM TO ENTER IN TABLE                    
*                                                                               
GFTRNMCN DS    0H                                                               
*                                                                               
         LLC   RF,CTFNMLEN         GET ELEMENT LENGTH                           
         LA    R6,0(RF,R6)         BUMP TO NEXT ELEMENT                         
         B     GFTRNMLP                                                         
*                                                                               
GFTRNMFD DS    0H                                                               
*                                                                               
         LTR   R0,R0               OKAY IF FIRST ENTRY ALREADY FOUND            
         BZ    *+8                                                              
         BCT   R0,GFTRCONT         ELSE COUNT DOWM TO 1ST ENTRY                 
*                                                                               
         MVC   FTRTNUM,CTEFKNUM    SAVE FEATURE NUMBER                          
*                                                                               
         AP    FTRCTR,=P'1'        BUMP TABLE ENTRIES COUNTER                   
*                                                                               
         CP    FTRCTR,SCRN#        STOP IF OVER A SCREEN                        
         BH    GFTRDONE                                                         
*                                                                               
         LLC   RF,CTFNMLEN         GET NAME LENGTH                              
         SHI   RF,CTFNM-CTFNMEL                                                 
         BNP   GFTRNM10            SKIP IF NO NAME                              
*                                                                               
         CHI   RF,L'FTRTNAME       MAKE SURE ITS NOT TOO LARGE                  
         BNH   *+8                                                              
         LHI   RF,L'FTRTNAME                                                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FTRTNAME(0),CTFNM   SAVE FEATURE NAME                            
*                                                                               
GFTRNM10 DS    0H                                                               
*                                                                               
         LA    R3,FTRTLENQ(R3)     BUMP TO NEXT TABLE ENTRY                     
*                                                                               
         XC    0(FTRTLENQ,R3),0(R3) INIT FIRST TABLE ENTRY                      
*                                                                               
GFTRNMDN DS    0H                                                               
*                                                                               
GFTRCONT DS    0H                                                               
*                                                                               
         GOTOR SEQ                 READ NEXT FEATURE RECORD                     
*                                                                               
         B     GFTRLOOP                                                         
*                                                                               
GFTRDONE DS    0H                                                               
*                                                                               
*        READ HIGHER RANKING VENDOR RECORDS                                     
*        AND RECORD THE FEATURES THEY USE                                       
*                                                                               
*        VENDOR MASTER RECORD                                                   
*                                                                               
GFTRVND  DS    0H                                                               
*                                                                               
         LA    R4,KEY              ESTABLISH VENDOR RECORD KEY                  
         USING CTEVKEY,R4          ESTABLISH VENDOR RECORD                      
         MVC   CTEVKEY,SVCTEVKY    COPY CUURENT RECORD KEY                      
*                                                                               
         XC    CTEVKNUL,CTEVKNUL   KILL LOWER LEVELS                            
*                                                                               
         GOTOR HIGH                READ VENDOR LEVEL RECORD                     
*                                                                               
         CLC   CTEVKEY,KEYSAVE     DONE IF NOT FOUND                            
         BNE   GFTRFTRX                                                         
*                                                                               
         GOTO1 GETREC              READ IN FOUND RECORD                         
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
*        SAVE VENDOR NAME                                                       
*                                                                               
         XC    SVVNDNM,SVVNDNM     INIT NAME                                    
*                                                                               
         LR    R6,R4               POINT TO RECORD START                        
         MVI   ELCODE,CTVDNELQ     VENDOR NAME     ELEMENT                      
         BRAS  RE,GETEL            FIND VENDOR NAME     ELM                     
         BNE   GFTRNAMX            SKIP IF NOT FOUND                            
*                                                                               
         USING CTVDND,R6           ESTABLISH VENDOR NAME ELEMENT                
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,CTVDNLEN       GET ELEMENT LENGTH                           
         SHI   RF,2                NAME LENGTH                                  
         BNP   GFTRNAMX            SKIP IF NONE                                 
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVVNDNM(0),CTVDN    SAVE VENDOR NAME                             
*                                                                               
         OC    SVVNDNM,SPACES      SPACE FILL                                   
*                                                                               
GFTRNAMX DS    0H                                                               
*                                                                               
         LR    R6,R4               POINT TO RECORD START                        
         MVI   ELCODE,CTEVRELQ     VENDOR FEATURES ELEMENT                      
         BRAS  RE,GETEL            FIND VENDOR FEATURES ELM                     
*                                                                               
GFTRFTLP DS    0H                                                               
*                                                                               
         BNE   GFTRFTDN            END OF FEATURE ELEMENTS                      
*                                                                               
         USING CTEVRD,R6           ESTABLISH VENDOR FEATURES ELM                
*                                                                               
         LA    R3,FTRTAB           POINT TO FEATURES TABLE                      
         USING FTRTABD,R3          ESTABLISH FEATURES TABLE                     
*                                                                               
GFTRVF1L DS    0H                                                               
*                                                                               
         OC    FTRTNUM,FTRTNUM     DONE AT END OF TABLE                         
         BZ    GFTRVF1D                                                         
*                                                                               
         CLC   FTRTNUM,CTEVFEAT    MATCH FEATURE TO ONE IN TABLE                
         BE    GFTRVF1F                                                         
*                                                                               
GFTRVF1C DS    0H                                                               
*                                                                               
         LA    R3,FTRTLENQ(R3)     BUMP TO NEXT TABLE ENTRY                     
         B     GFTRVF1L                                                         
*                                                                               
GFTRVF1F DS    0H                  MATCH TO TABLE FOUND                         
*                                                                               
         MVI   FTRTLVL,FTRTLVVQ    SET LEVEL FOR FEATURE                        
*                                                                               
GFTRVF1D DS    0H                                                               
*                                                                               
GFTRFTCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT FEATURE ELEMENT                    
*                                                                               
         B     GFTRFTLP            CONTINUE FEATURES SEARCH                     
*                                                                               
GFTRFTDN DS    0H                                                               
*                                                                               
GFTRVNDX DS    0H                                                               
*                                                                               
*        VENDOR PARTNER RECORD                                                  
*                                                                               
GFTRPTR  DS    0H                                                               
*                                                                               
         LA    R4,KEY              ESTABLISH VENDOR RECORD KEY                  
         MVC   CTEVKEY,SVCTEVKY    COPY CUURENT RECORD KEY                      
*                                                                               
         CLI   CTEVKARS,CTEVKRPQ   IF PARTNER RECORD TYPE                       
         BNE   GFTRPTRN                                                         
*                                                                               
         XC    CTEVKOFF,CTEVKOFF   KILL OFFICE LEVEL                            
*                                                                               
         OC    CTEVKUSC,CTEVKUSC   DONE IF NO PARTNER CODE                      
         BZ    GFTRPTRX                                                         
*                                                                               
         GOTOR HIGH                READ PARTNER LEVEL RECORD                    
*                                                                               
         CLC   CTEVKEY,KEYSAVE     DONE IF NOT FOUND                            
         BNE   GFTRPTRX                                                         
*                                                                               
         GOTOR GETREC              READ IN FOUND RECORD                         
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         LR    R6,R4               POINT TO RECORD START                        
         MVI   ELCODE,CTEVRELQ     VENDOR FEATURES ELEMENT                      
         BRAS  RE,GETEL            FIND VENDOR FEATURES ELM                     
*                                                                               
GFTRPRLP DS    0H                                                               
*                                                                               
         BNE   GFTRPRDN            END OF ELEMENTS                              
*                                                                               
         USING CTEVRD,R6           ESTABLISH VENDOR FEATURES ELM                
*                                                                               
         LA    R3,FTRTAB           POINT TO FEATURES TABLE                      
*                                                                               
GFTRPF1L DS    0H                                                               
*                                                                               
         OC    FTRTNUM,FTRTNUM     DONE AT END OF TABLE                         
         BZ    GFTRPF1D                                                         
*                                                                               
         CLC   FTRTNUM,CTEVFEAT    MATCH FEATURE TO ONE IN TABLE                
         BE    GFTRPF1F                                                         
*                                                                               
GFTRPF1C DS    0H                                                               
*                                                                               
         LA    R3,FTRTLENQ(R3)     BUMP TO NEXT TABLE ENTRY                     
         B     GFTRPF1L                                                         
*                                                                               
GFTRPF1F DS    0H                  MATCH TO TABLE FOUND                         
*                                                                               
         MVI   FTRTLVL,FTRTLVPQ    SET LEVEL FOR FEATURE                        
*                                                                               
GFTRPF1D DS    0H                                                               
*                                                                               
GFTRPRCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT FEATURE                            
*                                                                               
         BCT   RE,GFTRPRLP         CONTINUE TABLE SEARCH                        
*                                                                               
GFTRPRDN DS    0H                                                               
*                                                                               
*        VENDOR OFFICE RECORD                                                   
*                                                                               
GFTROFC  DS    0H                                                               
*                                                                               
         LA    R4,KEY              ESTABLISH VENDOR RECORD KEY                  
         MVC   CTEVKEY,SVCTEVKY    COPY CURRENT RECORD KEY                      
*                                                                               
         OC    CTEVKOFF,CTEVKOFF   DONE IF NO OFFICE CODE                       
         BZ    GFTROFCX                                                         
*                                                                               
         GOTOR HIGH                READ OFFICE LEVEL RECORD                     
*                                                                               
         CLC   CTEVKEY,KEYSAVE     DONE IF NOT FOUND                            
         BNE   GFTROFCX                                                         
*                                                                               
         GOTOR GETREC              READ IN FOUND RECORD                         
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         LR    R6,R4               POINT TO RECORD START                        
         MVI   ELCODE,CTEVRELQ     VENDOR FEATURES ELEMENT                      
*                                                                               
         BRAS  RE,GETEL            FIND VENDOR FEATURES ELM                     
*                                                                               
GFTROFLP DS    0H                                                               
*                                                                               
         BNE   GFTROFDN                                                         
*                                                                               
         USING CTEVRD,R6           ESTABLISH VENDOR FEATURES ELM                
*                                                                               
         LA    R3,FTRTAB           POINT TO FEATURES TABLE                      
*                                                                               
GFTROF1L DS    0H                                                               
*                                                                               
         OC    FTRTNUM,FTRTNUM     DONE AT END OF TABLE                         
         BZ    GFTROF1D                                                         
*                                                                               
         CLC   FTRTNUM,CTEVFEAT    MATCH FEATURE TO ONE IN TABLE                
         BE    GFTROF1F                                                         
*                                                                               
GFTROF1C DS    0H                                                               
*                                                                               
         LA    R3,FTRTLENQ(R3)     BUMP TO NEXT TABLE ENTRY                     
         B     GFTROF1L                                                         
*                                                                               
GFTROF1F DS    0H                  MATCH TO TABLE FOUND                         
*                                                                               
         MVI   FTRTLVL,FTRTLVOQ    SET LEVEL FOR FEATURE                        
*                                                                               
GFTROF1D DS    0H                                                               
*                                                                               
GFTROFCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT FEATURES ELM                       
*                                                                               
         BCT   RE,GFTROFLP         CONTINUE FEATURE SEARCH                      
*                                                                               
GFTROFDN DS    0H                                                               
*                                                                               
GFTROFCX DS    0H                                                               
*                                                                               
GFTRPTRX DS    0H                                                               
*                                                                               
         B     GFTRFTRX                                                         
*                                                                               
GFTRPTRN DS    0H                                                               
*                                                                               
*        LOCAL STATION  RECORD                                                  
*                                                                               
GFTRSTA  DS    0H                                                               
*                                                                               
         LA    R4,KEY              ESTABLISH VENDOR RECORD KEY                  
         MVC   CTEVKEY,SVCTEVKY    COPY CURRENT RECORD KEY                      
*                                                                               
         CLI   CTEVKARS,CTEVKLSQ   IF STATION RECORD TYPE                       
         BNE   GFTRSTAN                                                         
*                                                                               
         OC    CTEVKUID,CTEVKUID   DONE IF NO STATION CODE                      
         BZ    GFTRSTAX                                                         
*                                                                               
         GOTOR HIGH                READ AGENCY  LEVEL RECORD                    
*                                                                               
         CLC   CTEVKEY,KEYSAVE     DONE IF NOT FOUND                            
         BNE   GFTRSTAX                                                         
*                                                                               
         GOTOR GETREC              READ IN FOUND RECORD                         
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         LR    R6,R4               POINT TO RECORD START                        
         MVI   ELCODE,CTEVRELQ     VENDOR FEATURES ELEMENT                      
         BRAS  RE,GETEL            FIND VENDOR FEATURES ELM                     
*                                                                               
GFTRSTLP DS    0H                                                               
*                                                                               
         BNE   GFTRSTDN            END OF ELEMENTS                              
*                                                                               
         USING CTEVRD,R6           ESTABLISH VENDOR FEATURES ELM                
*                                                                               
         LA    R3,FTRTAB           POINT TO FEATURES TABLE                      
*                                                                               
GFTRSF1L DS    0H                                                               
*                                                                               
         OC    FTRTNUM,FTRTNUM     DONE AT END OF TABLE                         
         BZ    GFTRSF1D                                                         
*                                                                               
         CLC   FTRTNUM,CTEVFEAT    MATCH FEATURE TO ONE IN TABLE                
         BE    GFTRSF1F                                                         
*                                                                               
GFTRSF1C DS    0H                                                               
*                                                                               
         LA    R3,FTRTLENQ(R3)     BUMP TO NEXT TABLE ENTRY                     
         B     GFTRSF1L                                                         
*                                                                               
GFTRSF1F DS    0H                  MATCH TO TABLE FOUND                         
*                                                                               
         MVI   FTRTLVL,FTRTLVSQ    SET LEVEL FOR FEATURE                        
*                                                                               
GFTRSF1D DS    0H                                                               
*                                                                               
GFTRSTCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT FEATURE                            
*                                                                               
         BCT   RE,GFTRSTLP         CONTINUE TABLE SEARCH                        
*                                                                               
GFTRSTDN DS    0H                                                               
*                                                                               
GFTRSTAX DS    0H                                                               
*                                                                               
         B     GFTRFTRX                                                         
*                                                                               
GFTRSTAN DS    0H                                                               
*                                                                               
*        ROUTING AGENCY  RECORD                                                 
*                                                                               
GFTRAGY  DS    0H                                                               
*                                                                               
         LA    R4,KEY              ESTABLISH VENDOR RECORD KEY                  
         MVC   CTEVKEY,SVCTEVKY    COPY CURRENT RECORD KEY                      
*                                                                               
         CLI   CTEVKARS,CTEVKAGQ   IF AGENCY  RECORD TYPE                       
         BNE   GFTRAGYN                                                         
*                                                                               
         XC    CTEVKROF,CTEVKROF   KILL OFFICE LEVEL                            
*                                                                               
         OC    CTEVKRAG,CTEVKRAG   DONE IF NO AGENCY  CODE                      
         BZ    GFTRAGYX                                                         
*                                                                               
         GOTOR HIGH                READ AGENCY  LEVEL RECORD                    
*                                                                               
         CLC   CTEVKEY,KEYSAVE     DONE IF NOT FOUND                            
         BNE   GFTRAGYX                                                         
*                                                                               
         GOTOR GETREC              READ IN FOUND RECORD                         
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         LR    R6,R4               POINT TO RECORD START                        
         MVI   ELCODE,CTEVRELQ     VENDOR FEATURES ELEMENT                      
         BRAS  RE,GETEL            FIND VENDOR FEATURES ELM                     
*                                                                               
GFTRAGLP DS    0H                                                               
*                                                                               
         BNE   GFTRAGDN            END OF ELEMENTS                              
*                                                                               
         USING CTEVRD,R6           ESTABLISH VENDOR FEATURES ELM                
*                                                                               
         LA    R3,FTRTAB           POINT TO FEATURES TABLE                      
*                                                                               
GFTRAF1L DS    0H                                                               
*                                                                               
         OC    FTRTNUM,FTRTNUM     DONE AT END OF TABLE                         
         BZ    GFTRAF1D                                                         
*                                                                               
         CLC   FTRTNUM,CTEVFEAT    MATCH FEATURE TO ONE IN TABLE                
         BE    GFTRAF1F                                                         
*                                                                               
GFTRAF1C DS    0H                                                               
*                                                                               
         LA    R3,FTRTLENQ(R3)     BUMP TO NEXT TABLE ENTRY                     
         B     GFTRAF1L                                                         
*                                                                               
GFTRAF1F DS    0H                  MATCH TO TABLE FOUND                         
*                                                                               
         MVI   FTRTLVL,FTRTLVAQ    SET LEVEL FOR FEATURE                        
*                                                                               
GFTRAF1D DS    0H                                                               
*                                                                               
GFTRAGCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT FEATURE                            
*                                                                               
         BCT   RE,GFTRAGLP         CONTINUE TABLE SEARCH                        
*                                                                               
GFTRAGDN DS    0H                                                               
*                                                                               
*        ROUTING AGENCY OFFICE RECORD                                           
*                                                                               
GFTRROF  DS    0H                                                               
*                                                                               
         LA    R4,KEY              ESTABLISH VENDOR RECORD KEY                  
         MVC   CTEVKEY,SVCTEVKY    COPY CUURENT RECORD KEY                      
*                                                                               
         OC    CTEVKROF,CTEVKROF   DONE IF NO ROUTING OFFICE CODE               
         BZ    GFTRROFX                                                         
*                                                                               
         GOTOR HIGH                READ OFFICE LEVEL RECORD                     
*                                                                               
         CLC   CTEVKEY,KEYSAVE     DONE IF NOT FOUND                            
         BNE   GFTRROFX                                                         
*                                                                               
         GOTOR GETREC              READ IN FOUND RECORD                         
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         LR    R6,R4               POINT TO RECORD START                        
         MVI   ELCODE,CTEVRELQ     VENDOR FEATURES ELEMENT                      
*                                                                               
         BRAS  RE,GETEL            FIND VENDOR FEATURES ELM                     
*                                                                               
GFTRROLP DS    0H                                                               
*                                                                               
         BNE   GFTRRODN                                                         
*                                                                               
         USING CTEVRD,R6           ESTABLISH VENDOR FEATURES ELM                
*                                                                               
         LA    R3,FTRTAB           POINT TO FEATURES TABLE                      
*                                                                               
GFTRRF1L DS    0H                                                               
*                                                                               
         OC    FTRTNUM,FTRTNUM     DONE AT END OF TABLE                         
         BZ    GFTRRF1D                                                         
*                                                                               
         CLC   FTRTNUM,CTEVFEAT    MATCH FEATURE TO ONE IN TABLE                
         BE    GFTRRF1F                                                         
*                                                                               
GFTRRF1C DS    0H                                                               
*                                                                               
         LA    R3,FTRTLENQ(R3)     BUMP TO NEXT TABLE ENTRY                     
         B     GFTRRF1L                                                         
*                                                                               
GFTRRF1F DS    0H                  MATCH TO TABLE FOUND                         
*                                                                               
         MVI   FTRTLVL,FTRTLVRQ    SET LEVEL FOR FEATURE                        
*                                                                               
GFTRRF1D DS    0H                                                               
*                                                                               
GFTRROCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT FEATURES ELM                       
*                                                                               
         BCT   RE,GFTRROLP         CONTINUE FEATURE SEARCH                      
*                                                                               
GFTRRODN DS    0H                                                               
*                                                                               
GFTRROFX DS    0H                                                               
*                                                                               
GFTRAGYX DS    0H                                                               
*                                                                               
         B     GFTRFTRX                                                         
*                                                                               
GFTRAGYN DS    0H                                                               
*                                                                               
GFTRFTRX DS    0H                                                               
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         L     RF,AIO                                                           
         MVC   KEY,0(RF)           RESTORE FILE POINTERS                        
         GOTOR HIGH                                                             
*                                                                               
GETFTRSX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'TA0A59  DARE VENDOR RECORD - TSTNTRY'                           
***********************************************************************         
*                                                                     *         
*        TEST IF ANY FIELD ENTERED THIS TIME                          *         
*                                                                     *         
***********************************************************************         
*                                                                               
         DS    0D                  ALIGNMENT                                    
TSTNTRY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,SCRVNDRH         POINT TO FIRST FIELD ON SCREEN               
*                                                                               
TNTRLOOP DS    0H                                                               
*                                                                               
         CLI   0(R2),0             DONE AT END OF SCREEN                        
         BE    TNTRDONE                                                         
*                                                                               
         TM    1(R2),X'10'         SKIP IF PROTECTED FIELD                      
         BO    TNTRCONT                                                         
*                                                                               
         TM    4(R2),X'80'         TEST IF FIELD ENTERED THIS TIME              
         BO    TNTRNTRD                                                         
*                                                                               
TNTRCONT DS    0H                                                               
*                                                                               
         LLC   RF,0(R2)            FIELD LENGTH                                 
         LA    R2,0(RF,R2)         BUMP TO NEXT FIELD                           
         B     TNTRLOOP                                                         
*                                                                               
TNTRDONE DS    0H                  NO FIELD ENTERED THIS TIME                   
         CR    RB,RB               SET EQ CC                                    
         B     TSTNTRYX                                                         
*                                                                               
TNTRNTRD DS    0H                  SOME FIELD ENTERED                           
         LTR   RB,RB               SET NEQ CC                                   
*                                                                               
TSTNTRYX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'TA0A59  DARE VENDOR RECORD - FTRTABD'                           
***********************************************************************         
*                                                                     *         
*        DSECT FOR  FEATURES TABLE                                    *         
*                                                                     *         
***********************************************************************         
*                                                                               
FTRTABD  DSECT                                                                  
FTRTNUM  DS    XL2                 FEATURES NUMBER                              
FTRTNAME DS    CL28                FEATURES NAME                                
FTRTLVL  DS    XL1                 RECORD LEVEL                                 
FTRTLVVQ EQU   1                     VENDOR          RECORD LEVEL               
FTRTLVPQ EQU   2                     PARTNER         RECORD LEVEL               
FTRTLVOQ EQU   3                     OFFICE          RECORD LEVEL               
FTRTLVSQ EQU   4                     LOCAL   STATION RECORD LEVEL               
FTRTLVAQ EQU   5                     ROUTING AGENCY  RECORD LEVEL               
FTRTLVRQ EQU   6                     ROUTING OFFICE  RECORD LEVEL               
FTRTLENQ EQU   *-FTRTABD           LENGTH OF TABLE ENTRY                        
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM80D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM81D                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
SVVNDR   DS    CL10                VENDOR NUMBER SAVEAREA                       
SVVNDNM  DS    CL(L'SCRVNNM)       VENDOR NAME   SAVEAREA                       
SVPTR    DS    CL3                 PARTNER CODE                                 
SVAGY    DS    CL3                 ROUTING AGENCY CODE                          
SVSTA    DS    CL10                USERID  CODE                                 
SVARS    DS    CL1                 RECORD TYPE CODE                             
SVOFC    DS    CL2                 OFFICE  CODE                                 
COLSW    DS    XL1                 COLUMN NUMBER                                
SVLVL    DS    XL1                 VENDOR LEVEL                                 
         DS    0D                  ALIGNMENT                                    
SVPAGE   DS    PL8                 DISPLAY PAGE NUMBER                          
*                                                                               
LUPDATE  DS    CL14                BUFFER FOR DATE/TIME OF LAST UPDATE          
*                                                                               
X        DS    XL100                                                            
*                                                                               
SVCTEVKY DS    XL32                KEY SAVEAREA                                 
*                                                                               
*                                  NUMBER OF ENTRIES ON A PAGE                  
PAGE#Q   EQU   ((SCRCDLH-SCRCD01H)/(SCRCD02H-SCRCD01H))+1                       
                                                                                
         DS    0D                  ALIGNMENT                                    
SCRN#    DS    PL8                 # OF ENTRIES ON A PAGE                       
FTRCTR   DS    PL8                 # OF ENTRIES IN TABLE                        
*                                                                               
         DS    0D                  ALIGNMENT                                    
FTRTAB   DS    60XL(FTRTLENQ)      FEATURES TABLE                               
*                                                                               
         EJECT                                                                  
*                                                                               
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
LISTD    DSECT                                                                  
LSLINE   DS    0CL70                                                            
LSVND    DS    CL10                                                             
         DS    CL3                                                              
LSPTR    DS    CL3                                                              
         DS    CL3                                                              
LSSTA    DS    CL10                                                             
         DS    CL3                                                              
LSAGY    DS    CL3                                                              
         DS    CL3                                                              
LSOFC    DS    CL2                                                              
         DS    CL3                                                              
         DS    CL27                SPARE                                        
         EJECT                                                                  
* *******************                                                           
* ON-SCREEN FEATURE LINE                                                        
* *******************                                                           
FLLINED  DSECT                                                                  
FLSL01H  DS    XL(L'SCRSL01H)                                                   
FLSL01   DS    CL(L'SCRSL01)                                                    
FLCD01H  DS    XL(L'SCRCD01H)                                                   
FLCD01   DS    CL(L'SCRCD01)                                                    
FLFT01H  DS    XL(L'SCRFT01H)                                                   
FLFT01   DS    CL(L'SCRFT01)                                                    
FLSL21H  DS    XL(L'SCRSL21H)                                                   
FLSL21   DS    CL(L'SCRSL21)                                                    
FLCD21H  DS    XL(L'SCRCD21H)                                                   
FLCD21   DS    CL(L'SCRCD21)                                                    
FLFT21H  DS    XL(L'SCRFT21H)                                                   
FLFT21   DS    CL(L'SCRFT21)                                                    
FLSL02H  DS    0X                                                               
         EJECT                                                                  
* GEGENEDI                                                                      
* CTGENFILE                                                                     
* DDSPOOLD                                                                      
* DDCOMFACS                                                                     
* DDFLDIND                                                                      
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE GEGENEDI                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDACTIVD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'160CTSFM59   06/04/15'                                      
         END                                                                    
