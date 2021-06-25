*          DATA SET CTSFM5A    AT LEVEL 121 AS OF 03/12/19                      
*PHASE TA0A5AC                                                                  
*        TITLE 'TA0A5A  DARE PARTNER RECORD'                                    
*                                                                               
         TITLE 'TA0A5A  DARE PARTNER RECORD'                                    
**** CHANGE LOG                                                                 
*                                                                               
* BOBY   09/12  BIG BANG                                                        
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        TA0A5A - DARE PARTNER RECORD MAINT/LIST/REPORT        *         
*                                                                     *         
*  CALLED FROM  GENCON VIA TA0A00 (SFM CTFILE CONTROLLER)             *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, CHANGE, LIST, REPORT           *         
*                                                                     *         
*  INPUTS       SCREEN TA0A82 (MAINTENANCE)                           *         
*               SCREEN TA0A83 (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      UPDATED DARE PARTNER RECORD                           *         
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
         TITLE 'TA0A5A  DARE PARTNER RECORD - INIT'                             
TA0A5A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0A5A                                                         
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
         CLI   MODE,XRECADD        RECORD ADDED                                 
         BE    *+8                                                              
         CLI   MODE,XRECPUT        RECORD CHANGED                               
         BNE   *+8                                                              
         BRAS  RE,XR                  HANDLE PASSIVES                           
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
CKMODE20 DS    0H                                                               
         CLI   MODE,RECDEL                                                      
         BNE   CKMODE25                                                         
         LA    R2,CONACTH           NO DELETES                                  
         MVI   ERROR,INVRCACT                                                   
         GOTOR ERREX                                                            
         B     CKMODEX                                                          
*                                                                               
CKMODE25 CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   CKMODEX                                                          
         BRAS  RE,LR                                                            
         B     CKMODEX                                                          
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
*                                                                               
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
         TITLE 'TA0A5A  DARE PARTNER RECORD - VL'                               
***********************************************************************         
*                                                                     *         
*     VALIDATE KEY ROUTINE                                            *         
*                                                                     *         
***********************************************************************         
*                                                                               
VK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SVCTEPKY,SVCTEPKY        CLEAR KEY SAVEAREA                      
         LA    R4,SVCTEPKY                                                      
*                                                                               
         USING CTEPKEY,R4          ESTABLISH PARTNER KEY                        
*                                                                               
         MVI   CTEPKTYP,CTEPKTYQ   SET RECORD MAJOR TYPE                        
         MVI   CTEPKSTY,CTEPKSTQ   SET RECORD SECONDARY TYPE                    
*                                                                               
*        VALIDATE PARTNER CODE                                                  
*                                                                               
VKPTR    DS    0H                                                               
*                                                                               
         XC    SVPTNR,SVPTNR       INIT PARTNER CODE SAVEAREA                   
*                                                                               
         LA    R2,SCRPTNRH         POINT TO PARTNER CODE                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
         BNZ   VKPTR10                                                          
*                                  NO ENTRY                                     
         CLI   ACTNUM,ACTLIST      OK IF LIST                                   
         BE    VKPTRX                                                           
*                                                                               
         B     VKMISS              MISSING INPUT ERROR                          
*                                                                               
VKPTR10  DS    0H                  VALIDATE INPUT                               
*                                                                               
         MVC   SVPTNR,SPACES       INIT PARTNER CODE SAVEAREA                   
*                                                                               
         CLI   ACTNUM,ACTLIST      LIST RECORDS                                 
         BE    *+12                SKIP MINIMUM TEST                            
         CHI   RF,2                MINIMUM 2 CHARACTERS                         
         BL    VKNINV                                                           
*                                                                               
         CHI   RF,3                MAXIMUM 3 CHARACTERS                         
         BH    VKNINV                                                           
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVPTNR(0),8(R2)     SAVE PARTNER CODE                            
*                                                                               
*        VERIFY PARTNER IS NOT ALREADY USED AS REP ID                           
*                                                                               
VKPTRID  DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       ONLY ON ADDS                                 
         BNE   VKPTRX                                                           
*                                                                               
         MVC   SVPTNRKY,KEY        SAVE CURRENT KEY                             
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         LA    R5,KEY                                                           
         USING CTIREC,R5           ESTABLISH AS CTFILE ID REC                   
*                                                                               
         MVI   CTIKTYP,CTIKTYPQ    SET AS ID RECORD                             
*                                                                               
         MVC   CTIKID(L'SVPTNR),SVPTNR SET ID TO PARTNER CODE                   
         CLI   CTIKID+2,C' '       SET TRAILING SPACE TO NULLS                  
         BH    *+8                                                              
         MVI   CTIKID+2,0                                                       
*                                                                               
         MVC   SVCTIDKY,KEY        SAVE STARTING KEY                            
*                                                                               
         GOTOR DATAMGR,DMCB,=CL8'DMRDHI',=CL8'CTFILE',KEY,AIO3                  
*                                                                               
         L     R5,AIO3             POINT TO FOUND RECORD                        
*                                                                               
         LA    RF,CTIKID+3-CTIKEY  LENGTH OF COMPARE                            
*                                                                               
         CLI   CTIKTYP,CTIKTYPQ    OKAY IF NOT ID RECORD                        
         BNE   VKPTRIDX                                                         
*                                                                               
         CLI   SVPTNR+2,C' '       IF ONLY 2 CHARACTERS                         
         BH    *+8                                                              
         SHI   RF,1                   DECREMENT COMPARE LENGTH                  
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SVCTIDKY(0),CTIKEY  SEE IF PARTIAL MATCH FOUND                   
*                                                                               
         BNE   VKPTRID9               NONE - OKAY                               
*                                                                               
         CLI   SCROVR,C'Y'         OKAY IF OVERRIDE ENTERED                     
         BE    VKPTRID9                                                         
*                                                                               
         B     VKDUPE                                                           
*                                                                               
*        HIDE OVERRIDE FIELDS                                                   
*                                                                               
VKPTRID9 DS    0H                                                               
*                                                                               
         LA    RF,L'SCROVR         LENGTH OF OVERRIDE FIELD                     
         FOUT  SCROVRH,SPACES,(RF) CLEAR FIELD                                  
*                                                                               
         OI    SCROVRH+1,X'20'     PROTECT FIELD                                
         OI    SCROVRH+1,X'0C'     ZERO INTENSITY                               
*                                                                               
         LA    RF,L'SCROVRT                                                     
         FOUT  SCROVRTH,SCROVRT,(RF)                                            
*                                                                               
         OI    SCROVRTH+1,X'20'     PROTECT FIELD                               
         OI    SCROVRTH+1,X'0C'     ZERO INTENSITY                              
*                                                                               
         LA    RF,L'SCROVRC                                                     
         FOUT  SCROVRCH,SCROVRC,(RF)                                            
*                                                                               
         OI    SCROVRCH+1,X'20'     PROTECT FIELD                               
         OI    SCROVRCH+1,X'0C'     ZERO INTENSITY                              
*                                                                               
VKPTRIDX DS    0H                                                               
*                                                                               
         MVC   KEY,SVPTNRKY        RESTORE FILE POINTERS                        
*                                                                               
         GOTOR HIGH                                                             
*                                                                               
VKPTRX   DS    0H                                                               
*                                                                               
         MVC   CTEPKREP,SVPTNR     ADD PARTNER TO KEY                           
*                                                                               
VK90     DS    0H                                                               
*                                                                               
         MVC   KEY,CTEPKEY         SET KEY                                      
*                                                                               
VKX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
VKDUPE   DS    0H                  REPID ALREADY USED                           
*                                                                               
         LA    RF,L'SCROVR                                                      
         FOUT  SCROVRH,SCROVR,(RF)                                              
*                                                                               
         NI    SCROVRH+1,X'FF'-X'20'     OPEN FIELD                             
         NI    SCROVRH+1,X'FF'-X'0C'     NORMAL INTENSITY                       
*                                                                               
         LA    RF,L'SCROVRT                                                     
         FOUT  SCROVRTH,SCROVRT,(RF)                                            
*                                                                               
         NI    SCROVRTH+1,X'FF'-X'0C'     NORMAL INTENSITY                      
         OI    SCROVRTH+1,X'08'           HIGH   INTENSITY                      
*                                                                               
         LA    RF,L'SCROVRC                                                     
         FOUT  SCROVRCH,SCROVRC,(RF)                                            
*                                                                               
         NI    SCROVRCH+1,X'FF'-X'0C'     NORMAL INTENSITY                      
         OI    SCROVRCH+1,X'08'           HIGH   INTENSITY                      
*                                                                               
         LA    RF,REPIDER                                                       
*                                                                               
         B     VKERR                                                            
*                                                                               
VKMISS   LA    RF,MISSING          REQUIRED FIELD                               
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
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         STCM  RF,3,GTMSGNO        SET ERROR CODE                               
*                                                                               
         LA    R1,CTIKID           DISPLAY FOUND ID                             
         STCM  R1,7,GTATXT                                                      
*                                                                               
         LA    RF,L'CTIKID         MAX ID LENGTH                                
         LA    R1,0(RF,R1)         END OF ID                                    
         BCTR  R1,0                                                             
*                                                                               
         CLI   0(R1),C' '          FIND END OF ID                               
         BH    *+10                                                             
         BCTR  R1,0                BACK UP                                      
         BCT   RF,*-10                                                          
*                                                                               
         STC   RF,GTLTXT           SET ID LENGTH                                
*                                                                               
         GOTOR ERREX                                                            
*                                                                               
REPIDER  EQU   449               POSSIBLE REP MATCH TO EXISTING SIGNON          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,RE                                                            
*                                                                               
         TITLE 'TA0A5A  DARE PARTNER RECORD - DK'                               
***********************************************************************         
*                                                                     *         
*     DISPLAY  KEY ROUTINE                                            *         
*                                                                     *         
***********************************************************************         
*                                                                               
DK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AIO                                                           
         USING CTEPRECD,R4         ESTABLISH FEATURE RECORD                     
*                                                                               
*        DISPLAY KEY FIELDS                                                     
*                                                                               
         LA    RF,L'CTEPKREP       PARTNER CODE LENGTH                          
         FOUT  SCRPTNRH,CTEPKREP,(RF) DISPLAY PARTNER CODE                      
*                                                                               
         MVC   SVCTEPKY,CTEPKEY    SAVE KEY                                     
*                                                                               
DKX      DS    0H                                                               
         XIT1                      EXIT                                         
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'TA0A5A  DARE PARTNER RECORD - VR'                               
***********************************************************************         
*                                                                     *         
*     VALIDATE RECORD ROUTINE                                         *         
*                                                                     *         
***********************************************************************         
*                                                                               
VR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVCTEPKY,KEY        SAVE THE RECORD KEY                          
*                                                                               
         XC    SVPREL,SVPREL       INI ELEMENT SAVEAREA                         
*                                                                               
         L     R4,AIO                                                           
         USING CTEPRECD,R4         ESTABLISH FOUND RECORD                       
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF ACTION ADD                           
         BE    VR10                                                             
*                                                                               
*        FIND AND SAVE PREFIX                                                   
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,CTEPRELQ     FIND PARTNER ELEMENT                         
         BRAS  RE,GETEL                                                         
         BNZ   VR10                NONE FOUND                                   
*                                                                               
         USING CTEPRD,R6           ESTABLISH PARTNER ELM                        
*                                                                               
         LLC   RF,CTEPRLEN         GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVPREL(0),CTEPRD    SAVE PARTNER ELEMENT                         
*                                                                               
VR10     DS    0H                                                               
*                                                                               
         XC    CTEPRECD(100),CTEPRECD   INIT RECORD BUILD AREA                  
         MVC   CTEPKEY,KEY         SET KEY                                      
*                                                                               
         LA    RF,CTEPDAT-CTEPKEY  MINIMUM LENGTH OF RECORD                     
*                                                                               
         STCM  RF,3,CTEPFLEN       SET MINIMUM RECORD LENGTH                    
*                                                                               
         LA    R6,CTEPDAT          POINT TO FIRST ELEMENT                       
         USING CTEPRD,R6           ESTABLISH PARTNER ELEMENT                    
*                                                                               
         MVI   CTEPREL,CTEPRELQ    SET ELEMENT ID                               
*                                                                               
         MVI   CTEPRLEN,CTEPRLNQ   SET ELEMENT LENGTH                           
*                                                                               
         LA    RF,CTEPRLNQ(R6)     FORCE TRAILING NULL                          
         MVI   0(RF),0                                                          
*                                                                               
         SR    RF,R4               RECORD LENGTH                                
         STCM  RF,3,CTEPFLEN       SET RECORD LENGTH                            
*                                                                               
*        VALIDATE MEDIA                                                         
*                                                                               
VRMED    DS    0H                                                               
*                                                                               
         LA    R2,SCRMEDH          POINT TO MEDIA                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
         BZ    VRMISS              MISSING INPUT ERROR                          
*                                                                               
         LA    RF,1                MAX 1 BYTE FOR CHECKING                      
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         LA    R3,MEDTAB           START OF VALID MEDIAS                        
*                                                                               
VRMEDLP  DS    0H                                                               
*                                                                               
         USING MEDTABD,R3          ESTABLISH MEDIA TABLE                        
*                                                                               
         CLI   MEDTCDE,X'FF'       INVALID IF AT END OF TABLE                   
         BE    VRINV                                                            
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),MEDTCDE     COMPARE TO ENTRY IN TABLE                    
         BE    VRMEDFD             MATCH                                        
*                                                                               
VRMEDCN  DS    0H                                                               
         LA    R3,MEDTABLQ(R3)     BUMP TO NEXT ENTRY IN TABLE                  
         B     VRMEDLP                                                          
*                                                                               
VRMEDFD  DS    0H                  MATCH TO TABLE                               
*                                                                               
         MVC   SVMED,MEDTCDE       SAVE MEDIA CODE                              
         MVC   SVMEDNAM,MEDTNAM    SAVE MEDIA NAME                              
*                                                                               
         MVC   CTEPRMED,SVMED      ADD MEDIA TO ELM                             
*                                                                               
         LA    RF,L'MEDTNAM                                                     
         FOUT  (R2),MEDTNAM,(RF)   DISPLAY FULL NAME                            
*                                                                               
VRMEDX   DS    0H                                                               
*                                                                               
*        VALIDATE VENDOR                                                        
*                                                                               
VRVNDR   DS    0H                                                               
*                                                                               
         LA    RF,L'SCRVNNM        VENDOR NAME LENGTH                           
         FOUT  SCRVNNMH,SPACES,(RF) INIT VENDOR NAME                            
*                                                                               
*        VALIDATE VENDOR IS ON FILE                                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY              POINT TO SECONDARY IOA                       
         USING CTEVKEY,R5          ESTABLISH VENDOR KEY                         
*                                                                               
         MVI   CTEVKTYP,CTEVKTYQ   SET RECORD MAJOR TYPE                        
         MVI   CTEVKSTY,CTEVKSTQ   SET RECORD SECONDARY TYPE                    
*                                                                               
         LA    R2,SCRVNDRH         POINT TO VENDOR NAME FLD                     
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
         BZ    VRMISS              MUST BE ENTERED                              
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CTEVKVND(0),8(R2)   SAVE VENDOR CODE                             
*                                                                               
         LA    R0,CTEVKVND-CTEVKEY CALCULATE KEY CHECK LENGTH                   
         AR    R0,RF                  EXECUTE LENGTH                            
*                                                                               
         MVC   AIO,AIO2            READ INTO IOA2                               
*                                                                               
         GOTOR HIGH                READ VENDOR RECORDS                          
*                                                                               
VRVNDRLP DS    0H                                                               
*                                                                               
         LR    RF,R0               GET LENGTH FOR KEY CHECK                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   CTEVKEY(0),KEYSAVE  MATCH ON START KEY                           
         BNE   VRINV               NO MATCH                                     
*                                                                               
         CLI   CTEVKARS,0          MUST BE MASTER TYPE VENDOR                   
         BNE   VRVNDRCN                                                         
*                                                                               
         OC    CTEVKUSC,CTEVKUSC   NO PARTNER OR OFC ALLOWED                    
         BNZ   VRVNDRCN                                                         
         OC    CTEVKOFF,CTEVKOFF                                                
         BNZ   VRVNDRCN                                                         
*                                                                               
         B     VRVNDRFD                                                         
*                                                                               
VRVNDRCN DS    0H                                                               
*                                                                               
         GOTOR SEQ                 READ NEXT ON FILE                            
*                                                                               
         B     VRVNDRLP                                                         
*                                                                               
VRVNDRFD DS    0H                                                               
*                                                                               
         GOTOR GETREC              READ IN FOUND RECORD                         
*                                                                               
         MVC   CTEPVNAM,CTEVKVND   SAVE VENDOR CODE                             
*                                                                               
         LA    RF,L'CTEPVNAM       DISPLAY FULL VENDOR CODE                     
         FOUT  SCRVNDRH,CTEPVNAM,(RF)                                           
*                                                                               
         L     R6,AIO2             POINT TO VENDOR RECORD                       
         MVI   ELCODE,CTVDNELQ     FIND VENDOR NAME ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   VRVNDRX             NONE FOUND                                   
*                                                                               
         USING CTVDND,R6           ESTABLISH VENDOR NAME ELEMENT                
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,CTVDNLEN                                                    
         SHI   RF,2                LENGTH OF VENDOR NAME                        
*                                                                               
         FOUT  SCRVNNMH,CTVDN,(RF) DISPLAY VENDOR NAME                          
*                                                                               
         MVC   AIO,AIO1            RESTORE IOAREA POINTER                       
*                                                                               
VRVNDRX  DS    0H                                                               
*                                                                               
*        VALIDATE PARTNER NAME                                                  
*                                                                               
VRPTR    DS    0H                                                               
*                                                                               
         LA    R6,CTEPDAT          RE-POINT TO FIRST PARTNER ELEMENT            
         USING CTEPRD,R6           ESTABLISH PARTNER INFO ELEMENT               
*                                                                               
         LA    R2,SCRUSRNH         POINT TO PARTNER NAME                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
         BZ    VRMISS              INPUT REQUIRED                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CTEPPNAM(0),8(R2)   SAVE PREFIX NAME                             
*                                                                               
         OC    CTEPPNAM,SPACES     SPACE FILL                                   
*                                                                               
VRPTRX   DS    0H                                                               
*                                                                               
*        VALIDATE PARTNER PREFIX                                                
*                                                                               
VRPREF   DS    0H                                                               
*                                                                               
         LA    R2,SCRUSRPH         POINT TO PARTNER PREFIX                      
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
         BZ    VRMISS              MUST BE ENTERED                              
*                                                                               
******   TM    4(R2),X'04'         FIELD MUST BE ALPHABETIC                     
******   BNO   VRINV                                                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CTEPPREF(0),8(R2)   SAVE PREFIX NAME                             
*                                                                               
         OC    CTEPPREF,SPACES     SPACE FILL                                   
*                                                                               
         LA    R0,L'CTEPPREF       CALCULATE PPREFIX LENGTH                     
         LA    RF,CTEPPREF                                                      
         AR    RF,R0               POINT TO END OF USERID                       
         BCTR  RF,0                                                             
*                                                                               
         CLI   0(RF),C' '          FIND LAST NON-SPACE IN PREFIX                
         BH    *+10                                                             
         BCTR  RF,0                BACKUP A BYTE                                
         BCT   R0,*-10                                                          
*                                                                               
         AHI   RF,1                POINT TO END OF PREFIX                       
         LA    RE,CTEPPREF                                                      
         SR    RF,RE               LENGTH OF PREFIX                             
         STC   RF,CTEPLPRF         SAVE PREFIX LENGTH                           
*                                                                               
*        VALIDATE PREFIX IS UNIQUE                                              
*                                                                               
         CLC   CTEPPREF,CTEPPREF-CTEPREL+SVPREL                                 
         BE    VRPREFX             DONE IF PREFIX UNCHANGED                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING CTEQKEY,R5          ESTABLISH PREFIX PASSIVE                     
*                                                                               
         MVI   CTEQKTYP,CTEQKTYQ   SET RECORD MAJOR TYPE                        
         MVI   CTEQKSTY,CTEQKSTQ   SET RECORD SECONDARY TYPE                    
         MVC   CTEQKPAR,CTEPPREF   SET PREFIX IN KEY                            
*                                                                               
         GOTOR HIGH                READ PARTNER RECORDS                         
*                                                                               
         CLC   CTEQKEY(CTEQKUSC-CTEQKEY),KEYSAVE MATCH ON ID                    
         BE    VRINV                MATCH - DUPLICATE                           
*                                                                               
VRPREFOK DS    0H                                                               
*                                                                               
VRPREFX  DS    0H                                                               
*                                                                               
*        VALIDATE VERSION NUMBER NNN.NNN                                        
*                                                                               
VRVER    DS    0H                                                               
*                                                                               
         LA    R6,CTEPDAT          RE-POINT TO FIRST PARTNER ELEMENT            
         USING CTEPRD,R6           ESTABLISH PARTNER INFO ELEMENT               
*                                                                               
         LA    R2,SCRVERH          POINT TO VERSION FLD                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
         BZ    VRVERX              NOT REQUIRED                                 
*                                                                               
         LA    R1,8(RF,R2)         POINT TO LAST BYTE OF INPUT                  
         BCTR  R1,0                                                             
*                                                                               
         CLI   0(R1),C'.'          LOOK FOR DECIMAL POINT                       
         BE    *+14                                                             
         BCTR  R1,0                BACK UP A BYTE                               
         BCT   RF,*-10             CONTINUE SEARCH                              
         B     VRINV               MUST FIND A DECIMAL POINT                    
*                                                                               
         SHI   RF,1                DECREMENT FOR MAIN VERSION # LENGTH          
         BZ    VRINV               MUST HAVE MAIN VERSION #                     
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE LENGTH                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)         PACK MAIN VERSION #                          
*                                                                               
         TP    DUB                 MUST BE A PACKED NUMBER                      
         BNZ   VRINV                                                            
*                                                                               
         CVB   RE,DUB              CVB                                          
*                                                                               
         CHI   RE,255              MAX 255                                      
         BH    VRINV                                                            
*                                                                               
         STC   RE,CTEPVERS         SAVE MAIN VERSION #                          
*                                                                               
         SR    RF,RF                                                            
         LA    R1,1(R1)            FIRST BYTE OF LEVEL                          
         LR    R0,R1               SAVE STARTING POINT                          
*                                                                               
         CLI   0(R1),C' '          FIND END OF INPUT                            
         BNH   *+16                                                             
         AHI   RF,1                BUMP LENGTH COUNTER                          
         LA    R1,1(R1)            NEXT BYTE                                    
         B     *-16                TEST NEXT BYTE                               
*                                                                               
         SHI   RF,1                DECREMENT FOR EXECUTE                        
         BM    VRINV               MUST HAVE A LEVEL                            
*                                                                               
         LR    R1,R0               POINT TO FIRST BYTE OF LEVEL                 
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R1)         PACK LEVEL NUMBER                            
*                                                                               
         TP    DUB                 MUST BE A PACKED NUMBER                      
         BNZ   VRINV                                                            
*                                                                               
         CVB   RF,DUB              CVB                                          
*                                                                               
         CHI   RF,255              MAX 255                                      
         BH    VRINV                                                            
*                                                                               
         STC   RF,CTEPVERS+1       SAVE LEVEL NUMBER                            
*                                                                               
VRVERX   DS    0H                                                               
*                                                                               
*        VALIDATE FLAGS 1                                                       
*                                                                               
VRFLG    DS    0H                                                               
*                                                                               
         LA    R0,6                NUMBER OF FLAG FIELDS                        
         LA    R2,SCRFL01H         POINT TO FIRST FLAG FIELD                    
         LA    R3,FLGTAB           TABLE OF FLAGS                               
*                                                                               
VRFLGLP  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
         BZ    VRFLGCN             SKIP IF NOT ENTERED                          
*                                                                               
         CLI   8(R2),C'Y'          MUST BE Y OR S                               
         BE    *+8                                                              
         CLI   8(R2),C'S'          MUST BE Y OR S                               
         BE    *+8                                                              
         BNE   VRINV                                                            
*                                                                               
         OC    CTEPFLG1,0(R3)      ADD IN FLAG                                  
*                                                                               
VRFLGCN  DS    0H                                                               
*                                                                               
         LA    R2,(SCRFL02H-SCRFL01H)(R2) BUMP TO NEXT DESC FLD                 
         LA    R3,1(R3)            NEXT ENTRY IN FLAG TABLE                     
*                                                                               
         BCT   R0,VRFLGLP                                                       
*                                                                               
*        VALIDATE FLAGS 2                                                       
*                                                                               
VRFLG2   LA    R0,1                NUMBER OF FLAG FIELDS                        
         LA    R2,SCRFL09H         POINT TO FIRST FLAG FIELD                    
         LA    R3,FLGTAB2          TABLE OF FLAGS                               
*                                                                               
VRFLGLP2 DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
         BZ    VRFLGCN2            SKIP IF NOT ENTERED                          
*                                                                               
         CLI   8(R2),C'Y'          MUST BE Y OR S                               
         BE    *+8                                                              
         CLI   8(R2),C'S'          MUST BE Y OR S                               
         BE    *+8                                                              
         BNE   VRINV                                                            
*                                                                               
         OC    CTEPFLG2,0(R3)      ADD IN FLAG                                  
*                                                                               
VRFLGCN2 DS    0H                                                               
*                                                                               
         LA    R2,(SCRFL0AH-SCRFL09H)(R2) BUMP TO NEXT DESC FLD                 
         LA    R3,1(R3)            NEXT ENTRY IN FLAG TABLE                     
*                                                                               
         BCT   R0,VRFLGLP2                                                      
*                                                                               
VRFLGDN  DS    0H                                                               
*                                                                               
         MVC   KEY,SVCTEPKY        RESTORE KEY                                  
*                                                                               
         GOTOR HIGH                RESET FILE POINTERS                          
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF ADDING A RECORD                      
         BE    VR900                                                            
*                                                                               
         MVC   AIO,AIO2            READ OLD RECORD INTO AIO2                    
*                                  SETS UPDATE POINTERS                         
         GOTOR GETREC                                                           
*                                                                               
VR900    DS    0H                                                               
*                                                                               
         MVC   AIO,AIO1            RESET TO NEW VERSION OF RECORD               
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
         TITLE 'TA0A5A  DARE PARTNER RECORD - DR'                               
***********************************************************************         
*                                                                     *         
*     DISPLAY  RECORD ROUTINE                                         *         
*                                                                     *         
***********************************************************************         
*                                                                               
DR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AIO                                                           
         USING CTEPRECD,R4         ESTABLISH FEATURE RECORD                     
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,CTEPRELQ     FIND PARTNER ELEMENT                         
         BRAS  RE,GETEL                                                         
         BNZ   DRX                 NONE FOUND                                   
*                                                                               
         USING CTEPRD,R6           ESTABLISH PARTNER ELM                        
*                                                                               
*        DISPLAY MEDIA                                                          
*                                                                               
DRMED    DS    0H                                                               
*                                                                               
         LA    R3,MEDTAB           START OF VALID MEDIAS                        
*                                                                               
DRMEDLP  DS    0H                                                               
*                                                                               
         USING MEDTABD,R3          ESTABLISH MEDIA TABLE                        
*                                                                               
         CLI   MEDTCDE,X'FF'       DONE IF AT END OF TABLE                      
         BE    DRMEDDN                                                          
*                                                                               
         CLC   CTEPRMED,MEDTNAM    COMPARE TO ENTRY IN TABLE                    
         BE    DRMEDFD             MATCH                                        
*                                                                               
DRMEDCN  DS    0H                                                               
         LA    R3,MEDTABLQ(R3)     BUMP TO NEXT ENTRY IN TABLE                  
         B     DRMEDLP                                                          
*                                                                               
DRMEDFD  DS    0H                  MATCH TO TABLE                               
*                                                                               
         LA    RF,L'MEDTNAM                                                     
         FOUT  SCRMEDH,MEDTNAM,(RF)   DISPLAY FULL NAME                         
*                                                                               
DRMEDDN  DS    0H                  MATCH TO TABLE                               
*                                                                               
DRMEDX   DS    0H                                                               
*                                                                               
         LA    RF,L'CTEPVNAM       DISPLAY VENDOR                               
         FOUT  SCRVNDRH,CTEPVNAM,(RF)                                           
*                                                                               
         LA    RF,L'CTEPPNAM       DISPLAY PARTNER NAME                         
         FOUT  SCRUSRNH,CTEPPNAM,(RF)                                           
*                                                                               
         LA    RF,L'CTEPPREF       DISPLAY PARTNER PREFIX                       
         FOUT  SCRUSRPH,CTEPPREF,(RF)                                           
*                                                                               
*        DISPLAY VERSION NUMBER                                                 
*                                                                               
DRVER    DS    0H                                                               
*                                                                               
         LA    R2,SCRVERH          POINT TO VERSION FIELD                       
*                                                                               
         LA    RF,L'SCRVER         MAX FIELD LENGTH                             
         FOUT  (R2),SPACES,(RF)    INIT FIELD                                   
*                                                                               
         OC    CTEPVERS,CTEPVERS   SKIP IF NO VERSION NUMBER                    
         BZ    DRVERX                                                           
*                                                                               
         LA    R1,WORK             POINT TO WORKAREA                            
         XC    WORK,WORK           INIT WORKAREA                                
*                                                                               
         LLC   RF,CTEPVERS         MAIN VERSION NUMBER                          
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE EBCIDIC                                
*                                                                               
         LA    RF,0                ASSUME 1 DIGIT NUMBER                        
         CP    DUB,=P'10'                                                       
         BL    DRVER10                                                          
*                                                                               
         LA    RF,1                ASSUME 2 DIGIT NUMBER                        
         CP    DUB,=P'100'                                                      
         BL    DRVER10                                                          
*                                                                               
         LA    RF,2                ASSUME 3 DIGIT NUMBER                        
*                                                                               
DRVER10  DS    0H                                                               
*                                                                               
         SLL   RF,4                SHIFT TO LEFT NYBBLE                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  0(0,R1),DUB         DISPLAY MAIN VERSION #                       
*                                                                               
         SRL   RF,4                RETURN LENGTH TO RIGHT NYBBLE                
*                                                                               
         LA    R1,1(RF,R1)         POINT TO NEXT BYTE                           
         MVI   0(R1),C'.'          SET DECIMAL POINT                            
         LA    R1,1(R1)            BUMP OUTPUT POINTER                          
*                                                                               
         LLC   RF,CTEPVERS+1       LEVEL NUMBER                                 
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE EBCIDIC                                
*                                                                               
         LA    RF,0                ASSUME 1 DIGIT NUMBER                        
         CP    DUB,=P'10'                                                       
         BL    DRVER20                                                          
*                                                                               
         LA    RF,1                ASSUME 2 DIGIT NUMBER                        
         CP    DUB,=P'100'                                                      
         BL    DRVER20                                                          
*                                                                               
         LA    RF,2                ASSUME 3 DIGIT NUMBER                        
*                                                                               
DRVER20  DS    0H                                                               
*                                                                               
         SLL   RF,4                SHIFT TO LEFT NYBBLE                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  0(0,R1),DUB         DISPLAY MAIN VERSION #                       
*                                                                               
         SRL   RF,4                RETURN LENGTH TO RIGHT NYBBLE                
*                                                                               
         LA    R1,1(RF,R1)         POINT TO NEXT BYTE                           
*                                                                               
         LA    RF,WORK             START OF VERSION OUTPUT                      
         SR    R1,RF               OUTPUT LENGTH                                
         STC   R1,5(R2)            SET FIELD LENGTH                             
*                                                                               
         FOUT  (R2),WORK,(R1)      DISPLAY FIELD                                
*                                                                               
DRVERX   DS    0H                                                               
*                                                                               
* DISPLAY FLAG 1                                                                
*                                                                               
         LA    R0,6                MAX SIX FLAGS                                
         LA    R2,SCRFL01H         POINT TO FIRST FLAG FIELD                    
         LA    R3,FLGTAB           POINT TO FLAG TABLE                          
*                                                                               
DRFLGLP  DS    0H                                                               
*                                                                               
         LA    RF,L'SCRFL01        LENGTH OF FLAG FIELD                         
         FOUT  (R2),SPACES,(RF)    CLEAR FIELD                                  
*                                                                               
         MVC   FULL(1),0(R3)       COPY CURRENT FLAG                            
         NC    FULL(1),CTEPFLG1    CHECK IF FLAG IS ON                          
         BZ    DRFLGCN               NO                                         
*                                                                               
         MVI   FULL,C'Y'           INDICATE FLAG IS ON                          
         FOUT  (R2),FULL,1         DISPLAY FIELD                                
*                                                                               
DRFLGCN  DS    0H                                                               
*                                                                               
         LA    R2,(SCRFL02H-SCRFL01H)(R2) BUMP TO NEXT DESC FLD                 
         LA    R3,1(R3)            BUMP TO NEXT FLAGT ELM                       
*                                                                               
         BCT   R0,DRFLGLP          DISPLAY NEXT FLAG                            
*                                                                               
* DISPLAY FLAG 2                                                                
*                                                                               
         LA    R0,1                MAX SIX FLAGS                                
         LA    R2,SCRFL09H         POINT TO FIRST FLAG FIELD                    
         LA    R3,FLGTAB2          POINT TO FLAG TABLE                          
*                                                                               
DRFLGLP2 DS    0H                                                               
*                                                                               
         LA    RF,L'SCRFL01        LENGTH OF FLAG FIELD                         
         FOUT  (R2),SPACES,(RF)    CLEAR FIELD                                  
*                                                                               
         MVC   FULL(1),0(R3)       COPY CURRENT FLAG                            
         NC    FULL(1),CTEPFLG2    CHECK IF FLAG IS ON                          
         BZ    DRFLGCN2              NO                                         
*                                                                               
         MVI   FULL,C'Y'           INDICATE FLAG IS ON                          
         FOUT  (R2),FULL,1         DISPLAY FIELD                                
*                                                                               
DRFLGCN2 DS    0H                                                               
*                                                                               
         LA    R2,(SCRFL0AH-SCRFL09H)(R2) BUMP TO NEXT DESC FLD                 
         LA    R3,1(R3)            BUMP TO NEXT FLAGT ELM                       
*                                                                               
         BCT   R0,DRFLGLP2         DISPLAY NEXT FLAG                            
*                                                                               
*        DISPLAY VENDOR NAME                                                    
*                                                                               
DRVNNM   DS    0H                                                               
*                                                                               
         LA    RF,L'SCRVNNM        VENDOR NAME LENGTH FOUT                      
         FOUT  SCRVNNMH,SPACES,(RF) INIT VENDOR NAME                            
*                                                                               
*        READ VENDOR RECORD                                                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY              POINT TO SECONDARY IOA                       
         USING CTEVKEY,R5          ESTABLISH VENDOR KEY                         
*                                                                               
         MVI   CTEVKTYP,CTEVKTYQ   SET RECORD MAJOR TYPE                        
         MVI   CTEVKSTY,CTEVKSTQ   SET RECORD SECONDARY TYPE                    
         MVC   CTEVKVND,CTEPVNAM   SET VENDOR NAME                              
******   MVI   CTEVKARS,C'R'       FORCE VENDOR TYPE                            
*                                                                               
         LA    R2,SCRVNDRH         POINT TO VENDOR NAME FLD                     
         MVC   AIO,AIO2            READ INTO IOA2                               
*                                                                               
         GOTOR HIGH                READ VENDOR RECORDS                          
*                                                                               
         CLC   CTEVKEY,KEYSAVE     SKIP IF RECORD NOT FOUND                     
         BNE   DRVNNMX             NO MATCH                                     
*                                                                               
         GOTOR GETREC              READ FOUND RECORD                            
*                                                                               
         L     R6,AIO2             POINT TO FOUND RECORD                        
*                                                                               
         MVI   ELCODE,CTVDNELQ     FIND VENDOR NAME ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   DRVNNMX             NONE FOUND                                   
*                                                                               
         USING CTVDND,R6           ESTABLISH VENDOR NAME ELEMENT                
                                                                                
         SR    RF,RF                                                            
         ICM   RF,1,CTVDNLEN                                                    
         SHI   RF,2                LENGTH OF VENDOR NAME                        
*                                                                               
         FOUT  SCRVNNMH,CTVDN,(RF) DISPLAY VENDOR NAME                          
*                                                                               
         MVC   AIO,AIO1            RESTORE IOAREA POINTER                       
*                                                                               
DRVNNMX  DS    0H                                                               
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
         DROP  R4,R6                                                            
*                                                                               
         TITLE 'TA0A5A  DARE PARTNER RECORD - XR'                               
***********************************************************************         
*                                                                     *         
*     AFTER RECORD ADDED ROUTINE                                      *         
*                                                                     *         
***********************************************************************         
*                                                                               
XR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        WRITE PASSIVE POINTER                                                  
*                                                                               
         L     R4,AIO              POINT TO RECORD JUST ADDED/CHGD              
         USING CTEPRECD,R4         ESTABLSIH RECORD JUST ADDED/CHGD             
*                                                                               
         MVC   SVCTEPKY,CTEPKEY    SAVE KEY OF ADDED/CHGD REC                   
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING RECORD                             
         BNE   *+14                                                             
         MVC   SVCTEPDA,KEY           SAVE DISK ADDRESS                         
         B     *+10                                                             
         MVC   SVCTEPDA,CTEPDDA-CTEPKEY+KEY AFTER OTHER ACTIONS                 
*                                                                               
         LA    R4,SVCTEPKY         ESTABLISH ADDED/CHGD KEY                     
         USING CTEPRECD,R4                                                      
*                                                                               
         LA    R5,KEY              BUILD PASSIVE IN KEY                         
         USING CTEQRECD,R5         ESTABLISH PASSIVE                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   CTEQKTYP,CTEQKTYQ   SET RECORD MAJOR TYPE                        
         MVI   CTEQKSTY,CTEQKSTQ   SET RECORD SECONDARY TYPE                    
*                                                                               
         MVC   CTEQKUSC,CTEPKREP   COPY PARTNER CODE                            
*                                                                               
         MVC   CTEQDDA,SVCTEPDA    COPY DISK ADDRESS                            
*                                                                               
         L     R6,AIO              POINT TO ADDED/CHANGED RECORD                
         MVI   ELCODE,CTEPRELQ     FIND PARTNER ELEMENT                         
         BRAS  RE,GETEL                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CTEPRD,R6           ESTABLISH PARTNER ELEMENT                    
*                                                                               
         MVC   CTEQKPAR,CTEPPREF   SET PARTNER PREFIX                           
*                                                                               
         CLI   MODE,XRECPUT        SKIP IF NOT A CHANGE ACTION                  
         BNE   XR10                                                             
*                                                                               
*        DONE IF PARTNER PREFIX UNCHANGED                                       
*                                                                               
         CLC   CTEPLPRF,CTEPLPRF-CTEPREL+SVPREL                                 
         BNE   *+10                                                             
         CLC   CTEPPREF,CTEPPREF-CTEPREL+SVPREL                                 
         BE    XRX                                                              
*                                                                               
XR10     DS    0H                                                               
*                                                                               
*        ADD PASSIVE FOR PARTNER PREFIX                                         
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTOR HIGH                READ FOR ALREADY EXISTING                    
*                                                                               
         CLC   CTEQKEY,KEYSAVE     SKIP IF NOT FOUND                            
         BNE   XR20                                                             
*                                                                               
         TM    CTEQDSTA,X'80'      MUST BE DELETED                              
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL PASSIVE                     
*                                                                               
         GOTOR WRITE               RE-WRITE PASSIVE WITH NEW ADDRESS            
*                                                                               
         B     XR30                                                             
*                                                                               
XR20     DS    0H                                                               
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE NEW PASSIVE KEY                      
*                                                                               
         GOTOR ADD                 ADD PASSIVE                                  
*                                  ADD PASSIVE                                  
*                                                                               
XR30     DS    0H                                                               
*                                                                               
         CLI   ACTNUM,XRECADD      DONE IF ADDING RECORD                        
         BE    XRX                                                              
*                                                                               
*        DELETE OLD PASSIVE                                                     
*                                                                               
         MVC   CTEQKPAR,CTEPPREF-CTEPREL+SVPREL ADD OLD TO KEY                  
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTOR HIGH                SEE IF ALREADY ON FILE                       
*                                                                               
         CLC   CTEQKEY,KEYSAVE     DONE IF NOT FOUND                            
         BNE   XRX                                                              
*                                                                               
         OI    CTEQDSTA,X'80'      MARK DELETED                                 
*                                                                               
         GOTOR WRITE               RE-WRITE PASSIVE WITH NEW ADDR               
*                                                                               
XRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'TA0A5A  DARE PARTNER RECORD - LR'                               
***********************************************************************         
*                                                                     *         
*     LIST RECORDS ROUTINE                                            *         
*                                                                     *         
***********************************************************************         
*                                                                               
LR       NTR1  BASE=*,LABEL=*                                                   
         MVI   NLISTS,17           # OF LINES TO LIST, DEFAULT = 15             
         LA    R4,KEY                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR030               KEY IS LAST RECORD READ                      
*                                  SO GO CHECK VS. KEYSAVE                      
*                                                                               
*                                                                               
         USING CTEPKEY,R4          ESTABLISH FEATURE KEY                        
*                                                                               
         MVI   CTEPKTYP,CTEPKTYQ   SET RECORD MAJOR TYPE                        
         MVI   CTEPKSTY,CTEPKSTQ   SET RECORD SECONDARY TYPE                    
*                                                                               
         MVC   CTEPKREP,SVPTNR     SET PARTNER CODE IN KEY                      
*                                                                               
LR010    GOTO1 HIGH                                                             
*                                                                               
         B     LR030                                                            
*                                                                               
LR020    GOTO1 HIGH                RE-POINT FILE                                
*                                                                               
LR025    GOTO1 SEQ                                                              
*                                                                               
LR030    CLC   KEY(7),KEYSAVE      TEST FOR ALL DONE                            
         BNE   LR900                                                            
*                                                                               
         GOTOR GETREC              READ IN FOUND RECORD                         
*                                                                               
         L     R4,AIO              POINT TO RECORD                              
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R5,LISTAR                                                        
         USING LISTD,R5                                                         
*                                                                               
         MVC   LSPTR,CTEPKREP      SET PARTNER                                  
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,CTEPRELQ     FIND PRIMARY INFO ELM                        
         BRAS  RE,GETEL                                                         
         BNE   LR080                SKIP IF NONE                                
*                                                                               
         TM    CTEPFLG1,CTE1XMLR   X'08' - XML REP?                             
         BZ    *+8                                                              
         MVI   LSTYP,C'X'          XML REP                                      
*                                                                               
         TM    CTEPFLG1,CTE1TSTR   X'40' - TEST REP?                            
         BZ    *+8                                                              
         MVI   LSTYP+1,C'T'        TEST REP                                     
*                                                                               
         CLI   SCLMEDH+5,0         DO WE HAVE A MEDIA FILTER?                   
         BE    LR040               NO                                           
         CLC   SCLMED,CTEPRMED     YES, DO WE HAVE A MATCH?                     
         BNE   LR025                                                            
LR040    MVC   LSMED,CTEPRMED      MEDIA                                        
*                                                                               
         CLI   SCLVNDRH+5,0        DO WE HAVE A VENDOR FILTER?                  
         BE    LR050               NO                                           
         LLC   R1,SCLVNDRH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         CLC   SCLVNDR(0),CTEPVNAM  YES, DO WE MATCH A PORTION?                 
         BNE   LR025                                                            
LR050    MVC   LSVNDR,CTEPVNAM     VENDOR NAME                                  
*                                                                               
         MVC   LSVPTR,CTEPPNAM     PARTNER NAME                                 
         MVC   LSVPFX,CTEPPREF     PREFIX                                       
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
         DROP  R4,R6                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'TA0A5A  DARE PARTNER RECORD - TSTNTRY'                          
***********************************************************************         
*                                                                     *         
*        TEST IF ANY FIELD ENTERED THIS TIME                          *         
*                                                                     *         
***********************************************************************         
*                                                                               
         DS    0D                  ALIGNMENT                                    
TSTNTRY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,SCRPTNRH         POINT TO FIRST FIELD ON SCREEN               
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
         TITLE 'TA0A5A  DARE PARTNER RECORD - USRTAB'                           
***********************************************************************         
*                                                                     *         
*        TABLE FOR VALIDATING USER TYPES                              *         
*                                                                     *         
***********************************************************************         
*                                                                               
USRTAB   DS    0D                  TABLE OF VALID USER TYPES                    
         DC    CL1'B',CL8'BUYER   '   BUYER                                     
         DC    CL1'S',CL8'SELLER  '   SELLER                                    
         DC    X'FF'                  END OF TABLE                              
*                                                                               
         TITLE 'TA0A5A  DARE PARTNER RECORD - MEDTAB'                           
***********************************************************************         
*                                                                     *         
*        TABLE FOR VALIDATING MEDIA                                   *         
*                                                                     *         
***********************************************************************         
*                                                                               
MEDTAB   DS    0D                  TABLE OF VALID USER TYPES                    
         DC    CL1'T',CL10'TELEVISION'  TELEVISION                              
         DC    CL1'R',CL10'RADIO'       RADIO                                   
         DC    X'FF'                  END OF TABLE                              
*                                                                               
         TITLE 'TA0A5A  DARE PARTNER RECORD - FLGTAB'                           
***********************************************************************         
*                                                                     *         
*        TABLE FOR VALIDATING FLAGS                                   *         
*                                                                     *         
***********************************************************************         
*                                                                               
FLGTAB   DS    0D                  TABLE OF VALID FLAGS                         
         DC    XL1'80'                                                          
         DC    XL1'40'                                                          
         DC    XL1'20'                                                          
         DC    XL1'10'                                                          
         DC    XL1'08'                                                          
         DC    XL1'01'                                                          
                                                                                
FLGTAB2  DS    0D                  TABLE OF VALID FLAGS                         
         DC    XL1'80'                                                          
*                                                                               
         TITLE 'TA0A5A  DARE PARTNER RECORD - USRTABD'                          
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE FOR VALIDATING USER TYPES                    *         
*                                                                     *         
***********************************************************************         
*                                                                               
USRTABD  DSECT                                                                  
USRTCDE  DS    CL1                 USER TYPE CODE                               
USRTNAM  DS    CL8                 USER TYPE NAME                               
USRTABLQ EQU   *-USRTABD           LENGTH OF TABLE ENTRY                        
*                                                                               
         TITLE 'TA0A5A  DARE PARTNER RECORD - MEDTABD'                          
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE FOR VALIDATING MEDIAS                        *         
*                                                                     *         
***********************************************************************         
*                                                                               
MEDTABD  DSECT                                                                  
MEDTCDE  DS    CL1                 MEDIA CODE                                   
MEDTNAM  DS    CL10                MEDIA NAME                                   
MEDTABLQ EQU  *-MEDTABD            LENGTH OF TABLE ENTRY                        
*                                                                               
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM82D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM83D                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
SVPTNR   DS    CL3                 PARTNER CODE                                 
SVTYPE   DS    CL1                 USER TYPE                                    
SVMED    DS    CL1                 MEDIA                                        
SVMEDNAM DS    CL1                 MEDIA NAME                                   
SVVNDR   DS    CL10                VENDOR NAME                                  
SVUSRN   DS    CL10                USER NAME                                    
SVUSRCDE DS    CL1                 USER CODE                                    
SVUSRP   DS    CL10                USER PREFIX                                  
LUPDATE  DS    CL14                BUFFER FOR DATE/TIME OF LAST UPDATE          
X        DS    XL100                                                            
*                                                                               
SVPTNRKY DS    XL48                KEY SAVEAREA                                 
SVCTIDKY DS    XL48                KEY SAVEAREA                                 
SVCTEPKY DS    XL48                KEY SAVEAREA                                 
SVCTEPDA DS    XL4                 DISK ADDRESS SAVEAREA                        
*                                                                               
SVPREL   DS    XL256               PARTNER ELEMENT SAVEAREA                     
*                                                                               
         EJECT                                                                  
*                                                                               
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
LISTD    DSECT                                                                  
LSPTR    DS    CL3                                                              
         DS    CL2                                                              
LSTYP    DS    CL4                                                              
         DS    CL3                                                              
LSMED    DS    CL1                                                              
         DS    CL2                                                              
LSVNDR   DS    CL10                                                             
         DS    CL1                                                              
LSVPTR   DS    CL10                                                             
         DS    CL1                                                              
LSVPFX   DS    CL10                                                             
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
**PAN#1  DC    CL21'121CTSFM5A   03/12/19'                                      
         END                                                                    
