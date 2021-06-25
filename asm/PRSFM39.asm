*          DATA SET PRSFM39    AT LEVEL 067 AS OF 07/17/18                      
*PHASE T41C39A                                                                  
*        TITLE 'T41C39  DISCREPANCY COMMENT RECORDS'                            
*                                                                               
         TITLE 'T41C39  DISCREPANCY COMMENT RECORDS'                            
**** CHANGE LOG                                                                 
*                                                                               
* SMUR   04/18  SPEC-17729  NEW MEDIA (D)IGITAL AUDIO                           
*                                                                               
* BPLA   06/15  NEW MEDIA CODES B, V, W                                         
*                                                                               
* BOBY   09/08  BIG BANG                                                        
*                                                                               
         TITLE 'T41C39  DISCREPANCY COMMENT RECORDS '                           
***********************************************************************         
*                                                                     *         
*  TITLE        T41C39 - DISCREPANCY COMMENT RECORDS MAINT/LIST/REPRT *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM PRINT CONTROLLER)              *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, CHANGE, LIST, REPORT           *         
*                                                                     *         
*  INPUTS       SCREEN T41C9E (MAINTENANCE)                           *         
*               SCREEN T41C9F (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      UPDATED DISCREPANCY COMMENT RECORDS                   *         
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
         TITLE 'T41C39  DISCREPANCY COMMENT RECORDS - INIT'                     
T41C39   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C39                                                         
         L     RC,0(R1)            POINT TO GENERAL WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENERAL WORKAREA                   
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD            ESTABLISH SFM WORKING STORAGE                
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          ESTABLISH SPOOL WORKING STORAGE              
         USING SPOOLD,R8                                                        
*                                                                               
         OI    GENSTAT4,NODELLST   DELETE FROM LIST NOT ALLOWED                 
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   *+12                                                             
         BRAS  RE,VK                                                            
         B     INITX                                                            
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+12                                                             
         BRAS  RE,VR                                                            
         B     INITX                                                            
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BRAS  RE,DK                                                            
         B     INITX                                                            
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+12                                                             
         BRAS  RE,DR                                                            
         B     INITX                                                            
*                                                                               
         CLI   MODE,XRECADD        UPDATE PASSIVE RECORD                        
         BNE   *+12                                                             
         BRAS  RE,XR                                                            
         B     INITX                                                            
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   *+12                                                             
         BRAS  RE,LR                                                            
         B     INITX                                                            
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   *+12                                                             
         BRAS  RE,PR                                                            
         B     INITX                                                            
*                                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BNE   *+12                                                             
         BRAS  RE,DL                                                            
         B     INITX                                                            
*                                                                               
INITX    XIT1                                                                   
*                                                                               
         LA    R2,CONACTH           NO DELETES                                  
         MVI   ERROR,INVRCACT                                                   
         B     INITERR                                                          
*                                                                               
INITERR  DS    0H                                                               
         LA    R2,CONACTH           SECURITY LOCKOUT                            
         MVI   ERROR,SECLOCK                                                    
         B     INITER                                                           
*                                                                               
INITER   DS    0H                                                               
         GOTOR ERREX                                                            
*                                                                               
NOCHG    EQU   79                  FIELD CANNOT BE CHANGED                      
INVLEN   EQU   121                 LENGTH INVALID FOR DATA TYPE                 
*                                                                               
*                                  NUMBER OF COMMENT LINES ON SCREEN            
DCOMM#Q  EQU   1                                                                
*DCOMM#Q  EQU   (SCRDCMLH-SCRDCM1H)/(SCRDCM2H-SCRDCM1H)+1                       
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C39  DISCREPANCY COMMENT RECORDS - VALKEY'                   
***********************************************************************         
*                                                                     *         
*        VALIDATE KEY ROUTINE                                         *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC             ESTABLISH GENERAL WORKAREA                   
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
*                                                                               
*        MEDIA IS ONLY "A" FOR ALL MEDIA                                        
*                                                                               
         XC    SVDCMKEY,SVDCMKEY        CLEAR                                   
         LA    R4,SVDCMKEY                                                      
*                                                                               
         USING DCMKEY,R4           ESTABLISH DISCREPANCY COMMENT KEY            
*                                                                               
         MVC   DCMKAGY,AGENCY   CREATE KEY  -- AGENCY                           
         MVI   DCMKMED,C'A'                    ALWAYS "A"                       
         MVI   DCMKRCD,DCMKR1Q                 MAJOR ID                         
         MVI   DCMKRC2,DCMKR2Q                 MINOR ID                         
*                                                                               
*        VALIDATE DISCREPANCY COMMENT CODE                                      
*                                                                               
         XC    SVCODE,SVCODE       INIT SAVED DISCREPANCY CODE                  
*                                                                               
         LA    R2,SCRCODEH         COLUMN CODE FIELD ON SCREEN                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          IF NO INPUT                                  
         BNZ   VKCDE10                                                          
*                                                                               
         CLI   ACTNUM,ACTLIST         OK IF LIST                                
         BE    *+8                                                              
         CLI   ACTNUM,ACTREP          OK IF REPORT                              
         BE    VKCDEX                                                           
*                                                                               
         B     VKMISSER            ELSE ERROR - MISSING                         
*                                                                               
VKCDE10  DS    0H                                                               
*                                                                               
         CHI   RF,L'DCMKDCM        MUST NOT EXCEED COMMENT LENGTH               
         BH    VKLONGER                                                         
*                                                                               
         MVC   DCMKDCM,SCRCODE     COPY ENTERED CODE                            
         OC    DCMKDCM,SPACES      FORCE UPPER CASE                             
*                                                                               
*        NO INTERNAL BALNKS ALLOWED                                             
*                                                                               
         LA    R0,L'DCMKDCM        LENGTH OF CODE                               
         LA    R1,DCMKDCM          START OF CODE                                
*                                                                               
         CLI   0(R1),C' '          FIND FIRST SPACE                             
         BE    *+16                                                             
         LA    R1,1(R1)            BUMP POINTER                                 
         BCT   R0,*-12                                                          
         B     VKCDE20             NO SPACES                                    
*                                                                               
*        REST OF CODE MUST BE SPACES                                            
*                                                                               
         LA    R1,1(R1)                                                         
         BCT   R0,*+8                                                           
         B     VKCDE20             CODE ENDS IN SPACE                           
*                                                                               
         CLI   0(R1),C' '                                                       
         BNE   VKCDEER1            NO INTERNAL SPACES                           
         LA    R1,1(R1)            BUMP POINTER                                 
         BCT   R0,*-12                                                          
*                                                                               
VKCDE20  DS    0H                                                               
*                                                                               
         MVC   SVCODE,DCMKDCM      SAVE ENTERED CODE                            
*                                                                               
VKCDEX   DS    0H                                                               
*                                                                               
         MVC   KEY,SVDCMKEY        SET KEY                                      
*                                                                               
VKX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
VKMISSER LA    RF,MISSING                                                       
         B     VKERR                                                            
*                                                                               
VKCDEER1 LA    RF,BADCODE                                                       
         B     VKERR                                                            
*                                                                               
VKLONGER LA    RF,TOOLONG          INPUT TOO LONG                               
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
         TITLE 'T41C39  DISCREPANCY COMMENT RECORDS - DK'                       
***********************************************************************         
*                                                                     *         
*        DISPLAY KEY ROUTINE                                          *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
DK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC             ESTABLISH GENERAL WORKAREA                   
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
*                                                                               
         L     R4,AIO              POINT TO CURRENT RECORD                      
*                                                                               
         USING DCMKEY,R4           ESTABLISH DISCREPANCY COMMENT RECORD         
*                                                                               
         FOUT  SCRCODEH,DCMKDCM,12 DISPLAY DISCREPANCY CODE                     
*                                                                               
DKX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C39  DISCREPANCY COMMENT RECORDS - VR'                       
***********************************************************************         
*                                                                     *         
*        VALIDATE RECORD ROUTINE                                      *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC             ESTABLISH GENERAL WORKAREA                   
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
*                                                                               
         MVI   ACTELOPT,C'N'       SET FOR NO ACTIVITY ELEMS                    
*                                                                               
         MVC   SVDCMKEY,KEY        SAVE THE RECORD KEY                          
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
         USING DCMKEY,R4           ESTABLISH DISCREPANCY COMMENT RECORD         
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING                                    
         BNE   VR05                                                             
*                                                                               
         OI    SVCH1,PATVADDQ      INDICATE ADD ACTIVITY                        
*                                                                               
         XC    DCMRECD(256),DCMRECD     INIT RECORD BUILD AREA                  
         MVC   DCMKEY,KEY          SET KEY                                      
*                                                                               
         LA    RF,DCMFRST-DCMKEY   MINIMUM LENGTH OF RECORD                     
         LA    RF,DCMHELNQ(RF)     ADD ON HEADER ELEM LENGTH                    
*                                                                               
         STCM  RF,3,DCMRLEN        SET RECORD LENGTH                            
*                                                                               
         LA    R6,DCMFRST          POINT TO FIRST ELEMENT                       
*                                                                               
         USING DCMHELEM,R6         ESTABLISH HEADER ELM                         
*                                                                               
         MVI   DCMHELEM,DCMHELCQ   SET RECORD ID                                
         MVI   DCMHELEN,DCMHELNQ   SET ELEMENT LENGTH                           
*                                                                               
VR05     DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         BNE   *+8                                                              
         OI    SVCH1,PATVCHGQ         SET ACTIVITY INDICATOR                    
*                                                                               
         LA    R6,DCMFRST          POINT TO FIRST ELEMENT                       
         USING DCMHELEM,R6         ESTABLISH HEADER ELM                         
*                                                                               
         TITLE 'T41C39  DISCREPANCY COMMENT RECORDS - VRMEDS'                   
***********************************************************************         
*                                                                     *         
*        VALIDATE MEDIAS ENTRY - MEDIAS MUST BE IN MEDIA LIST         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRMEDS   DS    0H                  FLD=MEDIA                                    
*                                                                               
         MVC   SVMEDS,DCMHMEDS     SAVE OLD MEDIAS                              
*                                                                               
         XC    DCMHMEDS,DCMHMEDS   INIT FIELD                                   
*                                                                               
         LA    R2,SCRMEDH          POINT TO MEDIA FIELD                         
*                                                                               
         CLI   5(R2),0             NO INPUT IS THE SAME AS 'ALL'                
         BE    *+10                                                             
         CLC   =C'ALL',8(R2)                                                    
         BNE   *+14                                                             
         MVC   DCMHMEDS(3),=C'ALL'    SET FOR ALL MEDIA                         
         B     VRMEDS90            DONE WITH MEDIA                              
*                                                                               
         LLC   R0,5(R2)            INPUT LENGTH                                 
         LA    R1,8(R2)            POINT TO MEDIAS FIELD                        
         LA    RF,DCMHMEDS         MEDIA STORAGE AREA                           
*                                                                               
VRMED1LP DS    0H                                                               
*                                                                               
         CLI   0(R1),C' '          SKIP IF NOT ENTERED                          
         BNH   VRMED1CN                                                         
*                                                                               
         CLI   0(R1),C','          SKIP IF COMMA                                
         BE    VRMED1CN                                                         
*                                                                               
         LA    RE,MEDLIST          LIST OF VALID MEDIA                          
*                                                                               
VRMED2LP DS    0H                                                               
*                                                                               
         CLI   0(RE),C' '          DONE IF END OF LIST                          
         BNH   VRMED2DN                                                         
*                                                                               
         CLC   0(1,R1),0(RE)       MEDIA MUST BE IN LIST                        
         BE    VRMED2FD                                                         
*                                                                               
VRMED2CN DS    0H                                                               
*                                                                               
         LA    RE,1(RE)            BUMP TO NEXT ENTRY IN LIST                   
         B     VRMED2LP                                                         
*                                                                               
VRMED2DN DS    0H                  MEDIA NOT IN LIST - ERROR                    
*                                                                               
         B     VRMEDER                                                          
*                                                                               
VRMED2FD DS    0H                                                               
*                                                                               
         MVC   0(1,RF),0(RE)       ADD MEDIA TO LIST                            
         LA    RF,1(RF)            BUMP LIST POINTER                            
*                                                                               
VRMED1CN DS    0H                                                               
*                                                                               
         LA    R1,1(R1)            NEXT MEDIA IN LIST                           
         BCT   R0,VRMED1LP                                                      
*                                                                               
VRMED1DN DS    0H                                                               
*                                                                               
         OC    DCMHMEDS,DCMHMEDS   ERROR IF NO MEDIA FOUND                      
         BZ    VRMEDER             NO - INVALID                                 
*                                                                               
*        PUT MEDS IN ALPHABETICAL ORDER                                         
*                                                                               
         LA    R1,DCMHMEDS         COUNT NUMBER OF MEDIA ENTERED                
         SR    R0,R0                                                            
*                                                                               
         CLI   0(R1),C' '          FIND 1ST NON-MEDIA                           
         BNH   *+16                                                             
         LA    R1,1(R1)                                                         
         AHI   R0,1                                                             
         B     *-16                                                             
*                                                                               
         CHI   R0,1                SKIP IF ONLY ONE MEDIA                       
         BNH   VRMEDS90                                                         
*                                                                               
         LR    R1,R0               NUMBER OF MEDIA ENTERED                      
*                                                                               
         BCTR  R1,0                STARTING LOOP COUNTER                        
         LA    RE,DCMHMEDS                                                      
*                                                                               
VRSRT1LP DS    0H                                                               
*                                                                               
         LR    R0,R1               SET LOOP COUNTER                             
         LA    RF,1(RE)            START OF LOOP                                
*                                                                               
VRSRTLP  DS    0H                                                               
*                                                                               
         CLC   0(1,RE),0(RF)       IF 0(RE) GT 0(RF)                            
         BNH   VRSRTCN                                                          
*                                                                               
         XC    0(1,RE),0(RF)          SWAP THEM                                 
         XC    0(1,RF),0(RE)                                                    
         XC    0(1,RE),0(RF)                                                    
*                                                                               
VRSRTCN  DS    0H                                                               
*                                                                               
         LA    RF,1(RF)            BUMP POINTER                                 
         BCT   R0,VRSRTLP                                                       
*                                                                               
VRSRTDN  DS    0H                                                               
*                                                                               
VRSRT1CN DS    0H                                                               
*                                                                               
         LA    RE,1(RE)            BUMP START                                   
         BCT   R1,VRSRT1LP                                                      
*                                                                               
VRSRT1DN DS    0H                                                               
*                                                                               
VRMEDS90 DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       IF NOT ACTION ADD                            
         BE    VRMEDS10                                                         
*                                                                               
         CLC   SVMEDS,DCMHMEDS     IF MEDIAS HAS CHANGED                        
         BE    *+8                                                              
         OI    SVCH2,PDCMMEDQ         SET ACTIVITY INDICATOR                    
*                                                                               
VRMEDS10 DS    0H                                                               
*                                                                               
VRMEDSX  DS    0H                                                               
*                                                                               
         TITLE 'T41C39  DISCREPANCY COMMENT RECORDS - VRCTDT'                   
***********************************************************************         
*                                                                     *         
*        VALIDATE CUTOFF DATE                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRCTDT   DS    0H                                                               
*                                                                               
         LA    R6,DCMFRST          POINT TO FIRST ELEMENT                       
         USING DCMHELEM,R6         ESTABLISH HEADER ELM                         
*                                                                               
         MVC   SVCTDT,DCMHCTDT     SAVE PRIOR CUT OFF DATE                      
*                                                                               
         LA    R2,SCRCTDTH         POINT TO CUT OFF DATE                        
*                                                                               
         CLI   5(R2),0             OKAY IF NO DATE                              
         BNE   *+14                                                             
         XC    DCMHCTDT,DCMHCTDT                                                
         B     VRCTDT90                                                         
*                                                                               
         GOTOR DATVAL,DMCB,(0,8(R2)),DUB   VALIDATE DATE                        
         OC    0(4,R1),0(R1)       CHECK FOR ERRORS                             
         BZ    VRDATER                                                          
*                                                                               
         GOTOR DATCON,DMCB,(0,DUB),(3,DCMHCTDT)  STORE IN RECORD                
*                                                                               
         CLC   DCMHCTDT,BTODAY     MUST BE CURRENT OR FUTURE DATE               
         BL    VRDATER1                                                         
*                                                                               
VRCTDT90 DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       IF NOT ACTION ADD                            
         BE    VRCTDT10                                                         
*                                                                               
         CLC   SVCTDT,DCMHCTDT     IF DATE HAS CHANGED                          
         BE    *+8                                                              
         OI    SVCH2,PDCMCDTQ         SET ACTIVITY INDICATOR                    
*                                                                               
VRCTDT10 DS    0H                                                               
*                                                                               
VRCTDTX  DS    0H                                                               
*                                                                               
         TITLE 'T41C39  DISCREPANCY COMMENT RECORDS - VRDCM'                    
***********************************************************************         
*                                                                     *         
*        VALIDATE COMMENT                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRDCM    DS    0H                                                               
*                                                                               
*        CLEAR ELEMENT SAVEAREAS                                                
*                                                                               
         LA    R5,SVDCMELM         POINT TO COMMENT SAVEAREAS                   
         LA    R0,DCOMM#Q+2        MAX SAVEAREAS                                
*                                                                               
         XC    0(L'SVDCMELM,R5),0(R5) CLEAR SAVEAREA                            
         LA    R5,L'SVDCMELM(R5)   BUMP TO NEXT SAVEAREA                        
         BCT   R0,*-10                                                          
*                                                                               
*        REMOVE AND SAVE DCM ELMS FROM RECORD                                   
*                                                                               
         MVI   ELCODE,DCMCELCQ     SET FOR COMMENT ELMS                         
         LA    R5,SVDCMELM         POINT TO COMMENT SAVEAREA                    
*                                                                               
VRDREMLP DS    0H                                                               
*                                                                               
         GOTOR REMELEM             REMOVE ELEMENT                               
*                                                                               
         OC    ELEMENT,ELEMENT     DONE IF NO ELEMENT FOUND                     
         BZ    VRDREMDN                                                         
*                                                                               
         MVC   0(L'SVDCMELM,R5),ELEMENT SAVE COMMENT                            
*                                                                               
VRDREMCN DS    0H                                                               
*                                                                               
         LA    R5,L'SVDCMELM(R5)   BUMP TO NEXT SAVEAREA                        
         B     VRDREMLP            REMOVE NEXT COMMENT ELM                      
*                                                                               
VRDREMDN DS    0H                                                               
*                                                                               
*        ADD NEW COMMENTS TO RECORD                                             
*                                                                               
         LA    R6,ELEMENT          ESTABLISH ELEMENT BUILD AREA                 
         USING DCMCELEM,R6                                                      
*                                                                               
         LA    R5,SVDCMELM         POINT TO COMMENT SAVEAREAS                   
         LA    R0,DCOMM#Q          MAX NUMBER OF COMMENTS                       
         LA    R2,SCRDCM1H         POINT TO FIRST COMMENT LINE                  
         LA    R3,1                SET STARTING SEQUENCE NUMBER                 
*                                                                               
VRDADDLP DS    0H                                                               
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT BUILD AREA                      
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          GET ENTERED CODE LENGTH                      
         BZ    VRDADD10            SKIP IF NO ENTRY                             
*                                                                               
         MVI   DCMCELCD,DCMCELCQ   SET ELEMENT CODE                             
*                                                                               
         LA    RE,DCMCELNQ(RF)     COMMENT LENGTH PLUS HEADER LENGTH            
         STC   RE,DCMCELEN         SET ELEMENT LENGTH                           
*                                                                               
         STC   R3,DCMCSQN          SET ELEMENT SEQUENCE NUMBER                  
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DCMCCOMM(0),8(R2)   SAVE COMMENT                                 
*                                                                               
VRDADD10 DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       IF NOT ACTION ADD                            
         BE    VRDADD20                                                         
*                                                                               
         CLC   0(L'SVDCMELM,R5),ELEMENT IF ELEMENT CHANGED                      
         BE    *+8                                                              
         OI    SVCH2,PDCMDCMQ      INDICATE COMMENT CHANGED                     
*                                                                               
VRDADD20 DS    0H                                                               
*                                                                               
         GOTOR ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
VRDADDCN DS    0H                                                               
*                                                                               
         AHI   R5,L'SVDCMELM       BUMP TO NEXT SAVEAREA                        
         AHI   R3,1                INCREMENT SEQUENCE NUMBER                    
         BRAS  RE,BUMP             BUMP TO NEXT COMMENT FIELD                   
*                                                                               
         BCT   R0,VRDADDLP         NEXT COMMENT                                 
*                                                                               
VRDADDDN DS    0H                                                               
*                                                                               
VRDCMX   DS    0H                                                               
*                                                                               
         BRAS  RE,ACTVADD          ADD ACTIVITY ELEMENT                         
*                                                                               
         BRAS  RE,DR               RE-DISPLAY RECORD                            
*                                                                               
VRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
*        ERROR ROUTINES                                                         
*                                                                               
VRDATER  LHI   RF,INVDATE          INVALID DATE                                 
         B     VRERR                                                            
*                                                                               
VRDATER1 LHI   RF,OLDDATE          CUT OFF DATE NOT BEFORE TODAY                
         B     VRERR                                                            
*                                                                               
VRMEDER  LHI   RF,INVMED           INVALID MEDIA                                
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
MEDLIST  DC    C'BDILMNOSTVW '          LIST OF VALID MEDIA                     
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C39  DISCREPANCY COMMENT RECORDS - DR'                       
***********************************************************************         
*                                                                     *         
*        DISPLAY RECORD ROUTINE                                       *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
DR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC             ESTABLISH GENERAL WORKAREA                   
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
*                                                                               
         L     R4,AIO              EST FOUND DISCREPANCY COMMENT REC            
         USING DCMRECD,R4                                                       
*                                                                               
         LA    R6,DCMFRST          POINT TO FIRST ELEMENT                       
         USING DCMHELEM,R6         ESTABLISH HEADER ELEMENT                     
*                                                                               
*        DISPLAY VALID MEDIA                                                    
*                                                                               
DRMEDS   DS    0H                                                               
*                                                                               
         FOUT  SCRMEDH,SPACES,13  CLEAR FIELD                                   
*                                                                               
         CLC   =C'ALL',DCMHMEDS    IF ALL MEDIA                                 
         BNE   *+14                                                             
         MVC   SCRMED(3),=C'ALL'        DISPLAY 'ALL'                           
         B     DRMEDSX                                                          
*                                                                               
         LA    R1,DCMHMEDS         POINT TO LIST OF MEDIAS                      
         LA    RF,SCRMED           POINT TO MEDIAS FIELD                        
*                                                                               
DRMEDLP  DS    0H                                                               
*                                                                               
         CLI   0(R1),C' '          DONE IF NOT A LETTER                         
         BNH   DRMEDDN                                                          
*                                                                               
         MVC   0(1,RF),0(R1)       MOVE MEDIA TO LIST                           
         MVI   1(RF),C','          SEPARATING COMMA                             
*                                                                               
DRMEDCN  DS    0H                                                               
*                                                                               
         LA    R1,1(R1)            BUMP POINTERS                                
         LA    RF,2(RF)                                                         
*                                                                               
         B     DRMEDLP                                                          
*                                                                               
DRMEDDN  DS    0H                                                               
*                                                                               
         BCTR  RF,0                BACKUP TO LAST BYTE                          
         CLI   0(RF),C','          IF LAST CHARACTER IS A COMMA                 
         BNE   *+8                                                              
         MVI   0(RF),C' '          REMOVE IT                                    
*                                                                               
DRMEDSX  DS    0H                                                               
*                                                                               
*        DISPLAY CUTOFF DATE                                                    
*                                                                               
DRCTDT   DS    0H                                                               
*                                                                               
         FOUT  SCRCTDTH,SPACES,9   CLEAR FIELD                                  
*                                                                               
         GOTOR DATCON,DMCB,(3,DCMHCTDT),(17,SCRCTDT) DISPLAY DATE               
*                                                                               
DRCTDTX  DS    0H                                                               
*                                                                               
*        DISPLAY DISCREPANCY COMMENT                                            
*                                                                               
DRDCM    DS    0H                                                               
*                                                                               
         LA    R0,DCOMM#Q          NUMBER OF COMMENT LINES                      
         MVI   ELCODE,DCMCELCQ     LOOKING FOR COMMENT ELEMENTS                 
         LA    R6,DCMFRST          POINT TO FIRST ELEMENT                       
         LA    R2,SCRDCM1H         FIRST LINE OF COMMENTS                       
*                                                                               
DRDCMLP  DS    0H                                                               
*                                                                               
         USING DCMCELD,R6          ESTABLISH COMMENT ELEMENT                    
*                                                                               
         LA    R3,L'SCRDCM1        LENGTH OF SCREEN FIELD                       
         FOUT  (R2),SPACES,(R3)    CLEAR COMMENT LINE                           
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT COMMENT ELEMNT                     
         BNZ   DRDCMCN             NO MORE                                      
*                                                                               
         LLC   RF,DCMCELEN         GET ELEMENT LENGTH                           
         SHI   RF,DCMCELNQ         SUBTRACT HEADER LENGTH                       
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),DCMCCOMM    DISPLAY COMMENT                              
*                                                                               
DRDCMCN  DS    0H                                                               
*                                                                               
         BRAS  RE,BUMP             NEXT COMMENT FIELD                           
         BCT   R0,DRDCMLP          NEXT LINE OF COMMENT                         
*                                                                               
DRDCMDN  DS    0H                                                               
*                                                                               
*        DISPLAY ACTIVITY                                                       
*                                                                               
DRACTV   DS    0H                                                               
*                                                                               
         BRAS  RE,ACTVDIS                                                       
*                                                                               
DRAVTVX  DS    0H                                                               
*                                                                               
DRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C39  DISCREPANCY COMMENT RECORDS - XR'                       
***********************************************************************         
*                                                                     *         
*        AFTER RECORD ADDED                                           *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
XR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC             ESTABLISH GENERAL WORKAREA                   
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
*                                                                               
XRX      DS    0H                  NOTHING SPECIAL TO DO                        
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C39  DISCREPANCY COMMENT RECORDS - DL'                       
***********************************************************************         
*                                                                     *         
*        DELETE RECORD - NOT ALLOWED                                  *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
DL       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC             ESTABLISH GENERAL WORKAREA                   
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
*                                                                               
DLX      DS    0H                  NOTHING SPECIAL TO DO                        
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C39  DISCREPANCY COMMENT RECORDS - LR'                       
***********************************************************************         
*                                                                     *         
*        LIST RECORDS                                                 *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
LR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC             ESTABLISH GENERAL WORKAREA                   
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R4,KEY              EST. DISCREPANCY COMMENT REC                 
         MVC   AIO,AIO1                                                         
         USING DCMRECD,R4                                                       
*                                                                               
         LA    R5,LISTAR           ESTABLISH LIST LINE                          
         USING LISTD,R5                                                         
*                                                                               
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LRLOOP              KEY IS LAST RECORD READ                      
*                                  SO GO CHECK VS. KEYSAVE                      
*                                                                               
         MVC   DCMKAGY,AGENCY   CREATE KEY  -- AGENCY                           
*                                                                               
         MVI   DCMKMED,C'A'                    ALWAYS "A"                       
         MVI   DCMKRCD,DCMKR1Q                 MAJOR ID                         
         MVI   DCMKRC2,DCMKR2Q                 MINOR ID                         
         MVC   DCMKDCM,SVCODE                  DISCREPANCY COMMENT CODE         
*                                                                               
         GOTO1 HIGH                READ FIRST KEY                               
*                                                                               
LRLOOP   DS    0H                                                               
*                                                                               
         LA    R4,KEY              POINT TO KEY                                 
*                                                                               
         CLC   DCMKEY(DCMKDCM-DCMKEY),KEYSAVE  DONE ON CHG IN REC TYPE          
         BNE   LRDONE                                                           
*                                                                               
         GOTO1 GETREC              GET THE COL RECORD                           
*                                                                               
         L     R4,AIO              POINT TO RECORD                              
*                                                                               
         LA    R6,DCMFRST          POINT TO FIRST ELEMENT                       
         USING DCMHELEM,R6         ESTABLISH HEADER ELEMENT                     
*                                                                               
         MVC   LISTAR,SPACES       INIT LIST LINE                               
*                                                                               
         MVC   LISTCODE,DCMKDCM    DISPLAY DISCREPANCY CODE                     
*                                                                               
LRMEDS   DS    0H                                                               
*                                                                               
         CLC   =C'ALL',DCMHMEDS    IF ALL MEDIA                                 
         BNE   *+14                                                             
         MVC   LISTMEDS(3),=C'ALL'    DISPLAY 'ALL'                             
         B     LRMEDSX                                                          
*                                                                               
         LA    R1,DCMHMEDS         POINT TO LIST OF MEDIAS                      
         LA    RF,LISTMEDS         POINT TO MEDIAS FIELD                        
*                                                                               
LRMEDLP  DS    0H                                                               
*                                                                               
         CLI   0(R1),C' '          DONE IF NOT A LETTER                         
         BNH   LRMEDDN                                                          
*                                                                               
         MVC   0(1,RF),0(R1)       MOVE MEDIA TO LIST                           
         MVI   1(RF),C','          SEPARATING COMMA                             
*                                                                               
LRMEDCN  DS    0H                                                               
*                                                                               
         LA    R1,1(R1)            BUMP POINTERS                                
         LA    RF,2(RF)                                                         
*                                                                               
         B     LRMEDLP                                                          
*                                                                               
LRMEDDN  DS    0H                                                               
*                                                                               
         BCTR  RF,0                BACKUP TO LAST BYTE                          
*                                                                               
         CLI   0(RF),C','          IF LAST CHARACTER IS A COMMA                 
         BNE   *+8                                                              
         MVI   0(RF),C' '          REMOVE IT                                    
*                                                                               
LRMEDSX  DS    0H                                                               
*                                                                               
*        DISPLAY CUTOFF DATE                                                    
*                                                                               
LRCTDT   DS    0H                                                               
*                                                                               
         GOTOR DATCON,DMCB,(3,DCMHCTDT),(17,LISTCTDT) DISPLAY DATE              
*                                                                               
LRCTDTX  DS    0H                                                               
*                                                                               
*                                                                               
         GOTO1 LISTMON             SEND LINE TO SCREEN VIA GENCON               
*                                                                               
LRCONT   GOTO1 HIGH                RE-POINT GENFILE                             
*                                                                               
         GOTO1 SEQ                                                              
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
         TITLE 'T41C39  DISCREPANCY COMMENT RECORDS - PR'                       
***********************************************************************         
*                                                                     *         
*        PRINT REPORT                                                 *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
PR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC             ESTABLISH GENERAL WORKAREA                   
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R4,KEY              SET UP REGISTER FOR KEY IN DIRECTORY         
         MVC   AIO,AIO1                                                         
*                                                                               
         USING DCMRECD,R4                                                       
*                                                                               
         MVC   DCMKAGY,AGENCY   CREATE KEY  -- AGENCY                           
         MVI   DCMKMED,C'A'                    ALWAYS "A"                       
         MVI   DCMKRCD,DCMKR1Q                 MAJOR ID                         
         MVI   DCMKRC2,DCMKR2Q                 MINOR ID                         
         MVC   DCMKDCM,SVCODE                  DISCREPANCY COMMENT CODE         
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         B     PR30                                                             
*                                                                               
PR20     LA    R4,KEY                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
PR30     CLC   KEY(4),KEYSAVE      CHECK THRU RECORD CODE                       
         BNE   PR110               END OF RECORDS                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         GOTO1 GETREC              GET THE COL RECORD                           
*                                                                               
         L     R4,AIO              POINT TO RECORD                              
*                                                                               
         LA    R6,DCMFRST-DCMKEY(R4) POINT TO FIRST ELEMENT                     
*                                                                               
         USING DCMHELEM,R6         ESTABLISH HEADER ELM                         
*                                                                               
         MVI   RECFOUND,C'Y'       YES, FOUND DISCREPANCY COMMENT REC           
*                                                                               
         MVC   P1+00(12),DCMKDCM-DCMKEY(R4)                                     
*                                                                               
         LA    R1,P1+80                                                         
*                                                                               
         CLI   DCMHMEDS,X'FC'                                                   
         BNE   PR60                                                             
*                                                                               
         MVC   0(3,R1),=C'ALL'                                                  
         B     PR100               DONE                                         
*                                                                               
PR60     DS    0H                                                               
         TM    DCMHMEDS,X'80'                                                   
         BNO   *+14                                                             
         MVC   0(2,R1),=C'I,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
         TM    DCMHMEDS,X'40'                                                   
         BNO   *+14                                                             
         MVC   0(2,R1),=C'M,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
         TM    DCMHMEDS,X'20'                                                   
         BNO   *+14                                                             
         MVC   0(2,R1),=C'N,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
         TM    DCMHMEDS,X'10'                                                   
         BNO   *+14                                                             
         MVC   0(2,R1),=C'O,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
         TM    DCMHMEDS,X'08'                                                   
         BNO   *+14                                                             
         MVC   0(2,R1),=C'S,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
         TM    DCMHMEDS,X'04'                                                   
         BNO   *+14                                                             
         MVC   0(2,R1),=C'T,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
         AHI   R1,-1                                                            
         MVI   0(R1),C' '         CLEAR LAST COMMA                              
*                                                                               
PR100    DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR20                NEXT RECORD ENTRY                            
*                                                                               
PR110    CLI   RECFOUND,C'Y'       REPORT HAS DATA IN IT                        
         BE    PRX                                                              
         MVC   P1(16),=C'NO RECORDS FOUND'                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRX      XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
                                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
HOOKX    XIT1                                                                   
RECFOUND DC    X'0'                                                             
         SPACE 5                                                                
HEDSPECS SSPEC H1,1,C'     '                                                    
         SSPEC H1,42,C'PRINT DISCREPANCY COMMENT REPORT'                        
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
*                                                                               
         TITLE 'T41C39  DISCREPANCY COMMENT RECORDS - ACTVDIS'                  
***********************************************************************         
*                                                                     *         
*        DISPLAY ACTIVITY                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
ACTVDIS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
*        READ IN ALL ACTIVITY ELEMENTS                                          
*                                                                               
         LA    R2,SCRACT1H         POINT TO FIRST ACTIVITY LINE                 
*                                                                               
         LA    R0,((SCRACTL-SCRACT1)/(SCRACT2-SCRACT1))+1 # OF LINES            
*                                                                               
*        READ IN ACTIVITY ELEMENTS                                              
*                                                                               
         LAY   R4,SVDCMELM         POINT TO ACTIVITY ELMS SAVEAREA              
*                                                                               
         MVI   ELCODE,PATVELQ      SET FOR ACTIVITY ELEMENTS                    
         L     R6,AIO                                                           
         BRAS  RE,GETEL            FIND FIRST ELEMENT                           
*                                                                               
ACTDSVLP DS    0H                                                               
*                                                                               
         BNZ   ACTDSVDN            NONE                                         
*                                                                               
         MVC   0(L'SVDCMELM,R4),0(R6) SAVE ACTVITY ELEMENT                      
*                                                                               
         LA    R4,L'SVDCMELM(R4)   BUMP TO NEXT SAVEAREA                        
*                                                                               
ACTDSVCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT ACTIVITY ELEMENT                   
         B     ACTDSVLP                                                         
*                                                                               
ACTDSVDN DS    0H                                                               
*                                                                               
         LR    R6,R4               POINT TO END OF TABLE                        
*                                                                               
         LAY   R4,SVDCMELM         POINT TO FIRST SAVEAREA                      
*                                                                               
ACTDLOOP DS    0H                                                               
*                                                                               
         SHI   R6,L'SVDCMELM       BACK UP TO PREVIOUS ACTIVITY ELM             
*                                                                               
         CR    R6,R4               DONE IF PAST START OF ELEMENTS               
         BL    ACTDDONE                                                         
*                                                                               
         LA    R5,8(R2)            POINT TO DISPLAY AREA                        
         USING DSPACTVD,R5         ESTABLISH DISPLAY LINE                       
*                                                                               
         USING PATVELD,R6          ESTABLISH ACTIVITY ELEMENT                   
*                                                                               
         GOTOR TRNUSR,DMCB,(C'N',PATVUSR),(L'DSPUSR,DSPUSR)  DSP USERID         
*                                                                               
         GOTOR TRNPID,DMCB,(C'N',PATVPID),(L'DSPPID,DSPPID)  TRANS PID          
*                                                                               
         GOTOR DATCON,DMCB,(3,PATVDTE),(17,DSPDTE) DISPLAY DATE                 
*                                                                               
         LA    RF,ACTTBL           POINT TO ACTION TABLE                        
         LA    R1,DSPDATA-1        POINT TO DATA DISPLAY                        
*                                                                               
ACTDACLP DS    0H                                                               
*                                                                               
         CLI   0(RF),X'FF'         DONE AT END OF TABLE                         
         BE    ACTDACDN                                                         
*                                                                               
         MVC   BYTE,0(RF)          COPY INDICATOR IN TABLE                      
*                                                                               
         NC    BYTE,PATVCH1        CHECK IF ACTION PRESENT                      
         BZ    ACTDACCN                                                         
*                                                                               
         MVI   0(R1),C','          SET SEPARATOR                                
         MVC   1(3,R1),1(RF)       SET TRANSLATION                              
*                                                                               
         LA    R1,4(R1)            BUMP TO NEXT DISPLAY AREA                    
*                                                                               
ACTDACCN DS    0H                                                               
*                                                                               
         LA    RF,4(RF)            NEXT ITEM IN TABLE                           
         B     ACTDACLP                                                         
*                                                                               
ACTDACDN DS    0H                                                               
*                                                                               
         LA    RF,DATTBL           POINT TO DATA TABLE                          
*                                                                               
ACTDDALP DS    0H                                                               
*                                                                               
         CLI   0(RF),X'FF'         DONE AT END OF TABLE                         
         BE    ACTDDADN                                                         
*                                                                               
         MVC   BYTE,0(RF)          COPY INDICATOR IN TABLE                      
*                                                                               
         NC    BYTE,PATVCH2        CHECK IF DATA PRESENT                        
         BZ    ACTDDACN                                                         
*                                                                               
         MVI   0(R1),C','          SET SEPARATOR                                
         MVC   1(4,R1),1(RF)       SET TRANSLATION                              
*                                                                               
         LA    R1,5(R1)            BUMP TO NEXT DISPLAY AREA                    
*                                                                               
         CLI   0(R1),C' '          BACKUP TO FIRST NON-BLANK                    
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         LA    R1,1(R1)            FIRST TRAILING SPACE                         
*                                                                               
ACTDDACN DS    0H                                                               
*                                                                               
         LA    RF,5(RF)            NEXT ITEM IN TABLE                           
         B     ACTDDALP                                                         
*                                                                               
ACTDDADN DS    0H                                                               
*                                                                               
         MVI   DSPDATA-1,C' '      REMOVE ANY COMMA                             
*                                                                               
ACTDCONT DS    0H                                                               
*                                                                               
         BRAS  RE,BUMP             BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,ACTDLOOP                                                      
*                                                                               
ACTDDONE DS    0H                                                               
*                                                                               
ACTVDISX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
ACTTBL   DS    0X                  TABLE OF ACTIONS                             
         DC    X'80',C'ADD'        ADD                                          
         DC    X'40',C'DEL'        DELETE                                       
         DC    X'20',C'RES'        RESTORE                                      
         DC    X'10',C'CHA'        CHANGE                                       
         DC    X'FF'               EOT                                          
*                                                                               
DATTBL   DS    0X                  TABLE OF DATA CHANGED                        
         DC    X'80',CL4'MEDS'     MEDIAS                                       
         DC    X'40',CL4'DATE'     DO NOT USE DATE                              
         DC    X'20',CL4'COMM'     COMMENT                                      
         DC    X'FF'               EOT                                          
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM38 - EXCHNAGE RECORD MAINT - BUMP'                         
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
         TITLE 'PRSFM38 - EXCHNAGE RECORD MAINT - TRNUSR'                       
***********************************************************************         
*                                                                     *         
*        TRANSLATE USR TO A NAME                                      *         
*                                                                     *         
*NTRY    R2 ==>   SCREEN FIELD                                        *         
*        P1+0     C'N' - NOT DISPLAYING TO SCREEN FIELD               *         
*        P1       A(USR)                                              *         
*        P2+0(1)  L'RETURN AREA  IF R2 = 0                            *         
*        P2+1(3)  A(RETURN AREA) IF R2 = 0                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TRNUSR   NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   SAVKEY,KEY          SAVE CURRENT KEY                             
         MVC   SAVAIO,AIO          SAVE CURRENT AIO                             
*                                                                               
         L     R5,0(R1)            POINT TO USR                                 
*                                                                               
         MVC   BYTE,4(R1)          SAVE LENGTH OF RETURN AREA                   
         L     R3,4(R1)            POINT TO RETURN AREA                         
*                                                                               
         CLI   0(R1),C'N'          IF R2 POINTS TO SCREEN FIELD                 
         BE    *+8                                                              
         LA    R3,8(R2)               USE DATAAREA OF SCREEN FIELD              
*                                                                               
         OC    0(2,R5),0(R5)       SKIP IF NO USR FOUND                         
         BZ    TUSRNOTF                                                         
*                                                                               
*        READ ID RECORD ON CTFILE                                               
*                                                                               
         LA    R4,KEY                                                           
         USING CTIREC,R4           ESTABLISH KEY AS ID RECORD                   
         XC    CTIKEY,CTIKEY       INIT KEY                                     
*                                                                               
         MVI   CTIKTYP,CTIKTYPQ    SET RECORD TYPE                              
*                                                                               
         MVC   CTIKNUM,0(R5)       SET USR                                      
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         CLC   CTIKEY,KEYSAVE      SKIP IF RECORD NOT FOUND                     
         BNE   TUSRNOTF                                                         
*                                                                               
*        FIND ID                                                                
*                                                                               
*        FIND PERSON'S ID ELEMENT                                               
*                                                                               
         LA    R6,CTIDATA          POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
TUSRCTLP DS    0H                                                               
*                                                                               
         CLI   0(R6),0             CHECK FOR END OF RECORD                      
         BE    TUSRCTDN                                                         
*                                                                               
         CLI   0(R6),CTDSCELQ      FIND DESCRIPTION ELEMENT                     
         BE    TUSRCTFD                                                         
*                                                                               
TUSRCTCN DS    0H                                                               
*                                                                               
         IC    RF,1(R6)            GET ELEMENT LENGTH                           
         AR    R6,RF               BUMP TO NEXT ELEMENT                         
         B     TUSRCTLP            GO FIND NEXT ELEMENT                         
*                                                                               
TUSRCTDN DS    0H                  NO PERSON ID FOUND                           
*                                                                               
         B     TUSRNOTF                                                         
*                                                                               
TUSRCTFD DS    0H                                                               
*                                                                               
         USING CTDSCD,R6          ESTABLISH DESCRIPTION ELEMENTT                
*                                                                               
         SR    RF,RF                                                            
         IC    RF,CTDSCLEN         GET ELEMENT LENGTH                           
*                                                                               
         SHI   RF,CTDSC-CTDSCD     SUBTRACT HEADER LENGTH                       
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),CTDSC       GET USERID                                   
*                                                                               
         B     TUSRDSP                                                          
*                                                                               
TUSRNOTF DS    0H                  PRINT 'UNKNOWN' IF NO USR                    
*                                                                               
         MVC   WORK(7),=CL7'UNKNOWN'                                            
*                                                                               
TUSRDSP  DS    0H                                                               
*                                                                               
*        OUTPUT USERID                                                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,BYTE           GET OUTPUT LENGTH                            
         BZ    TRNUSRX             NO OUTPUT AREA                               
*                                                                               
         BCTR  RF,0                   DECREMENT FOR EXECUTE                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK           INIT OUT PUT AREA                         
*                                                                               
         MVC   KEY,SAVKEY          RESTORE CURRENT KEY                          
         MVC   AIO,SAVAIO          RESTORE CURRENT AIO                          
*                                                                               
         GOTOR HIGH                RESTORE FILE POINTERS                        
*                                                                               
TRNUSRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41D30 - INVOICE COMMENTS MAINT/LIST - TRNPID'                  
***********************************************************************         
*                                                                     *         
*        TRANSLATE PID TO A NAME                                      *         
*                                                                     *         
*NTRY    R2 ==>   SCREEN FIELD                                        *         
*        P1+0     C'N' - NOT DISPLAYING TO SCREEN FIELD               *         
*        P1       A(PID)                                              *         
*        P2+0(1)  L'RETURN AREA  IF R2 = 0                            *         
*        P2+1(3)  A(RETURN AREA) IF R2 = 0                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TRNPID   NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   SAVKEY,KEY          SAVE CURRENT KEY                             
         MVC   SAVAIO,AIO          SAVE CURRENT AIO                             
*                                                                               
         L     R5,0(R1)            POINT TO PID                                 
*                                                                               
         SR    R6,R6                                                            
         IC    R6,4(R1)            SAVE LENGTH OF RETURN AREA                   
         L     R3,4(R1)            POINT TO RETURN AREA                         
*                                                                               
         CLI   0(R1),C'N'          IF R2 POINTS TO SCREEN FIELD                 
         BE    *+8                                                              
         LA    R3,8(R2)               USE DATAAREA OF SCREEN FIELD              
*                                                                               
         OC    0(2,R5),0(R5)       SKIP IF NO PID FOUND                         
         BZ    TPIDNOTF                                                         
*                                                                               
*        READ PERSON AUTH REC ON CTFILE                                         
*                                                                               
         LA    R4,KEY                                                           
         USING CT0REC,R4           ESTABLISH KEY AS PERSON AUTH REC             
         XC    CT0KEY,CT0KEY       INIT KEY                                     
*                                                                               
         MVI   CT0KTYP,CT0KTEQU    SET RECORD TYPE                              
         MVC   CT0KAGY,SAVSECAG    SET SECURITY AGENCY                          
*                                                                               
         CLC   CT0KAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   CT0KAGY,AGENCY         USE BUYREC'S AGENCY                       
*                                                                               
         MVC   CT0KNUM,0(R5)       SET PID                                      
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         CLC   CT0KEY,KEYSAVE      SKIP IF RECORD NOT FOUND                     
         BNE   TPIDNOTF                                                         
*                                                                               
*        FIND USER'S ID                                                         
*                                                                               
*        FIND PERSON'S ID ELEMENT                                               
*                                                                               
         LA    RE,CT0DATA          POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
TPIDCTLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDCTDN                                                         
*                                                                               
         CLI   0(RE),X'C3'         - MATCH ON ELEMENT CODE                      
         BE    TPIDCTFD                                                         
*                                                                               
TPIDCTCN DS    0H                                                               
*                                                                               
         IC    RF,1(RE)            GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDCTLP            GO FIND NEXT ELEMENT                         
*                                                                               
TPIDCTDN DS    0H                  NO PERSON ID FOUND                           
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDCTFD DS    0H                                                               
*                                                                               
*        FIND PERSON RECORD                                                     
*                                                                               
         LA    R4,KEY                                                           
         USING SAPEREC,R4          ESTABLISH KEY AS PERSON REC KEY              
         XC    SAPEKEY,SAPEKEY     INIT KEY                                     
*                                                                               
         MVI   SAPETYP,SAPETYPQ    SET RECORD TYPE                              
         MVI   SAPESUB,SAPESUBQ    SET RECORD SUB TYPE                          
*                                                                               
         MVC   SAPEAGY,SAVSECAG    SET SECURITY AGENCY                          
*                                                                               
         CLC   SAPEAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   SAPEAGY,AGENCY         USE BUYREC'S AGENCY                       
*                                                                               
         MVC   SAPEPID,2(RE)       SET USERID FROM PREVIOUS RECORD              
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
*                                                                               
         L     R4,AIO2             POINT TO FOUND RECORD                        
*                                                                               
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),KEYSAVE SKIP IF REC NOT FOUND           
         BNE   TPIDNOTF                                                         
*                                                                               
         LA    RE,SAPEDATA         POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
*        FIND NAME ELEMENT                                                      
*                                                                               
TPIDNMLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDNMDN                                                         
*                                                                               
         USING SANAMD,RE           ESTABLISH AS NAME ELEMENT                    
*                                                                               
         CLI   SANAMEL,SANAMELQ    LOOKING FOR NAME ELEMENT                     
         BE    TPIDNMFD                                                         
*                                                                               
TPIDNMCN DS    0H                                                               
*                                                                               
         IC    RF,SANAMLN          GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDNMLP            GO PROCESS NEXT ELEMENT                      
*                                                                               
TPIDNMDN DS    0H                  NAME ELEMENOT FOUND                          
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDNMFD DS    0H                                                               
*                                                                               
         SR    R0,R0               GET ELEMENT LENGTH                           
         IC    R0,SANAMLN                                                       
         AHI   R0,-SANAMLNQ        DECRMENT BY FIXED LENGTH                     
         BNP   TPIDNOTF            NO NAME IN ELEMENT                           
*                                                                               
         LA    RE,SANAMES          POINT TO START OF PERSON'S NAME              
         SR    RF,RF                                                            
         LA    R1,WORK             BUILD NAME IN WORKAREA                       
         XC    WORK,WORK                                                        
*                                                                               
TPIDFMLP DS    0H                  FORMAT PERSON'S NAME                         
*                                                                               
         USING SANAMES,RE          ESTABLISH NAMES SECTION                      
*                                                                               
         IC    RF,SANAMELN         GET LENGTH OF THIS PART OF NAME              
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SANAME      MOVE OUT PART OF NAME                        
*                                                                               
         LA    R1,1(RF,R1)         BUMP TO NEXT OUTPUT AREA                     
*                                                                               
TPIDFMCN DS    0H                                                               
*                                                                               
         SR    R0,RF               DECREMENT REMAINING ELEMENT LENGTH           
         AHI   R0,-2               FOR NAME LENGTH BYTE & EX LENGTH             
         BNP   TPIDFMDN              END OF ELEMENT REACHED                     
*                                                                               
         LA    RE,2(RF,RE)         POINT TO NEXT PART OF NAME                   
         LA    R1,1(R1)            ADD IN A SPACING CHARACTER                   
*                                                                               
         B     TPIDFMLP                                                         
*                                                                               
TPIDFMDN DS    0H                                                               
*                                                                               
         B     TPIDSQSH                                                         
*                                                                               
TPIDNOTF DS    0H                  PRINT 'UNKNOWN' IF NO PID                    
*                                                                               
         MVC   WORK(7),=CL7'UNKNOWN'                                            
         LA    R1,WORK+7           POINT TO NEXT OUTPUT POSITION                
*                                                                               
TPIDSQSH DS    0H                                                               
*                                                                               
         LR    R0,R1               END OF OUTPUT MINUS START                    
         LA    RF,WORK             START OF WORKAREA                            
         SR    R0,RF               EQUALS OUTPUT LENGTH                         
*                                                                               
         GOTO1 SQUASHER,DMCB,WORK,(R0) SQUASH NAME                              
*                                                                               
*        MOVE NAME TO SCREEN                                                    
*                                                                               
         LR    RF,R6                  GET RETURN AREA LENGTH                    
         BCTR  RF,0                   DECREMENT FOR EXECUTE                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES         INIT OUT PUT AREA                         
*                                                                               
         L     RF,4(R1)               SAVE SQUASHED LENGTH                      
*                                                                               
         CR    RF,R6                  IF NAME TOO LONG                          
         BNH   *+6                                                              
         LR    RF,R6                     USE MAX FOR RETURN AREA                
*                                                                               
         B     TPIDMVC                                                          
*                                                                               
TPIDMVC  DS    0H                                                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK        DISPLAY NAME                                 
*                                                                               
         MVC   KEY,SAVKEY          RESTORE CURRENT KEY                          
         MVC   AIO,SAVAIO          RESTORE CURRENT AIO                          
*                                                                               
         GOTOR HIGH                RESTORE FILE POINTERS                        
*                                                                               
TRNPIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C39  DISCREPANCY COMMENT - ACTVADD'                          
***********************************************************************         
*                                                                     *         
*        ADD ACTIVITY ELEMENT                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
ACTVADD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
*        DELETE ALL ACTIVITY ELEMENTS                                           
*                                                                               
         MVI   ELCODE,PATVELQ      SET ACTVITY ELEMENT CODE                     
*                                                                               
         SR    R0,R0               INIT ELEMENT COUNTER                         
         LA    R6,SVDCMELM         POINT TO ACTIVITY ELM SAVEAREA               
         XC    0(L'SVDCMELM,R6),0(R6)   INIT FIRST SAVEAREA                     
*                                                                               
ATVREMLP DS    0H                                                               
*                                                                               
         GOTOR REMELEM             REMOVE A CURRENT ACTIVTY ELEMENT             
*                                                                               
         OC    ELEMENT,ELEMENT     DONE IF NONE FOUND                           
         BZ    ATVREMDN                                                         
*                                                                               
         AHI   R0,1                BUMP ELEMENT COUNTER                         
*                                  REMELEM RETURNS DEL'D ELM IN ELEMENT         
         MVC   0(L'SVDCMELM,R6),ELEMENT   SAVE DELETED ELEMENT                  
*                                  REMELEM RETURNS DEL'D ELM IN ELEMENT         
         LA    R6,L'SVDCMELM(R6)          POINT TO NEXT SAVEAREA                
         XC    0(L'SVDCMELM,R6),0(R6)     INIT NEXT SAVEAREA                    
*                                                                               
ATVREMCN DS    0H                                                               
*                                                                               
         B     ATVREMLP                                                         
*                                                                               
ATVREMDN DS    0H                                                               
*                                                                               
*        FIND PID                                                               
*                                                                               
         XC    SAVPID,SAVPID       PASSWORD ID NUMBER CLEARED                   
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
*                                                                               
         GOTO1 CGETFACT,DMCB,(2,0),0,0   RETURN TIME IN TUS                     
*                                                                               
         DROP  RF                                                               
*                                                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
*                                                                               
         MVC   SAVSECAG,FATAGYSC   ALSO NEEDED TO GET CORRECT PID               
*                                                                               
         TM    FATFLAG,X'08'       CHECK IF SECET CODE IS THERE                 
         BZ    *+10                                                             
         MVC   SAVPID,FAPASSWD     SAVE PASSWORD ID NUMBER                      
*                                                                               
*        SEE IF USER MADE PREVIOUS CHANGE TODAY                                 
*                                                                               
         LA    R6,SVDCMELM         POINT TO FIRST ACTIVITY ELEMENT              
         USING PATVELD,R6          ESTABLISH ACTIVITY ELEMENT                   
*                                                                               
         SR    R5,R5               INIT SEQ SAVEAREA                            
         SR    R3,R3               INIT A(HIGHEST SQN ELM)                      
*                                                                               
ATVCHKLP DS    0H                                                               
*                                                                               
         CLI   PATVELCD,0          DONE IF SLOT EMPTY                           
         BE    ATVCHKDN                                                         
*                                                                               
         CLM   R5,3,PATVSQN        FIND HIGHEST SEQUENCE NUMBER                 
         BNL   *+8                                                              
         ICM   R5,3,PATVSQN                                                     
*                                                                               
         CLC   PATVDTE,BTODAY      MATCH ON DATE                                
         BNE   ATVCHKCN                                                         
*                                                                               
         CLC   PATVPID,SAVPID      MATCH ON PID                                 
         BNE   ATVCHKCN                                                         
*                                                                               
         CLC   PATVUSR,TWAORIG     MATCH ON USERID                              
         BNE   ATVCHKCN                                                         
*                                                                               
         LR    R3,R6               SAVE A(TABLE ENTRY)                          
*                                                                               
ATVCHKCN DS    0H                                                               
         LA    R6,L'SVDCMELM(R6)   BUMP TO NEXT SAVED ELEMENT                   
         B     ATVCHKLP                                                         
*                                                                               
ATVCHKDN DS    0H                  NO PRIOR ACTIVITY BY USER TODAY              
*                                                                               
         LTR   R3,R3               SKIP IF NO OTHER CHANGES TODAY               
         BZ    ATVCHKD1                                                         
*                                                                               
         CLM   R5,3,PATVSQN-PATVELD(R3) MUST BE LATEST ELM                      
         BNE   *+10                                                             
         LR    R6,R3               POINT TO FOUND ELEMENT                       
         B     ATVCHKFD                                                         
*                                                                               
ATVCHKD1 DS    0H                                                               
*                                                                               
*        BUILD NEW ACTIVITY ELEMENT IN TABLE                                    
*                                                                               
         MVI   PATVELCD,PATVELQ    SET ELEMENT CODE                             
         MVI   PATVLEN,PATVHDRL+2  SET ELEMENT LENGTH                           
*                                                                               
         AHI   R5,1                BUMP HIGHEST SEQUENCE NUMBER                 
         STCM  R5,3,PATVSQN        SET ELM SQN                                  
*                                                                               
         MVC   PATVUSR,TWAORIG     SET USER ID                                  
         MVC   PATVDTE,BTODAY      SET DATE                                     
         MVC   PATVPID,SAVPID      SET PID                                      
*                                                                               
         AHI   R0,1                BUMP NUMBER OF ELEMENTS                      
*                                                                               
ATVCHKFD DS    0H                                                               
*                                                                               
         OC    PATVCH1,SVCH1       ADD IN CHANGED DATA BITS                     
         OC    PATVCH2,SVCH2                                                    
*                                                                               
*        CLEAR EXTRA ACTIVITY ELEMENTS                                          
*                                                                               
         CHI   R0,DCMACT#Q         OKAY IF UNDER MAX ACTIVITY ELEMENTS          
         BNH   ATVCLRDN                                                         
*                                                                               
         LAY   R6,SVDCMELM         POINT TO TABLE OF ACTIVITY ELMS              
*                                                                               
ATVCLRLP DS    0H                                                               
*                                                                               
         CLI   PATVELCD,0          DONE IF END OF ELEMENTS                      
         BE    ATVCLRDN                                                         
*                                                                               
*                                  SKIP IF ADD OR DELETE OR RESTORE             
         TM    PATVCH1,PATVADDQ+PATVDELQ+PATVRSTQ                               
         BNZ   ATVCLRCN                                                         
*                                                                               
         XC    0(L'SVDCMELM,R6),0(R6)     CLEAR ENTRY                           
         SHI   R0,1                DECREMENT NUMBER OF ELEMENTS                 
*                                                                               
         CHI   R0,DCMACT#Q         DONE IF MAX ELMS LEFT                        
         BNH   ATVCLRDN                                                         
*                                                                               
ATVCLRCN DS    0H                                                               
*                                                                               
         LA    R6,SVDCMELM(R6)          NEXT ELEMENT IN TABLE                   
         B     ATVCLRLP                                                         
*                                                                               
ATVCLRDN DS    0H                                                               
*                                                                               
*        ADD ACTIVITY ELEMENTS TO RECORD                                        
*                                                                               
         OI    GENSTAT5,ADDEQCOD   ADD ELMS AT END OF ELEMENT TYPE              
*                                                                               
         LA    R6,SVDCMELM         POINT TO SAVED ACTIVITY ELMS                 
*                                                                               
ATVADDLP DS    0H                                                               
*                                                                               
         CLI   0(R6),0             SKIP IF EMPTY SLOT                           
         BE    ATVADDCN                                                         
*                                                                               
         MVC   ELEMENT,0(R6)       GET NEXT SAVED ELEMENT                       
*                                                                               
         GOTOR ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
         SHI   R0,1                DECREMENT ELEMENT COUNTER                    
         BZ    ATVADDDN            NONE LEFT                                    
*                                                                               
ATVADDCN DS    0H                                                               
*                                                                               
         LA    R6,L'SVDCMELM(R6)   BUMP TO NEXT ELEMENT                         
*                                                                               
         B     ATVADDLP            ADD NEXT ACTVITY ELEMENT                     
*                                                                               
ATVADDDN DS    0H                                                               
*                                                                               
ACTVADDX DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFM9ED                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFM9FD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
SVCODE   DS    CL10                                                             
SVMEDS   DS    XL20                MEDIA CODES SAVEAREA                         
SVCTDT   DS    XL3                 CUTOFF DATE SAVEAREA                         
SVCH1    DS    XL1                 ACTION INDICATOR                             
SVCH2    DS    XL1                 ACTIVITY INDICATOR 1                         
         DS    XL6                 ROOM FOR MORE ACTIVITY INDICATORS            
SAVPID   DS    XL2                 PID                                          
SAVSECAG DS    CL2                 SECURITY AGENCY                              
*                                                                               
         DS    0D                                                               
SAVKEY   DS    XL32                KEY SAVEAREA                                 
SAVAIO   DS    XL4                 AIO SAVEAREA                                 
*                                                                               
OLDDATE  EQU   604                 CUT OFF DATE NOT BEFORE TODAY                
BADCODE  EQU   605                 CODE HAS SPACES IN IT                        
*                                                                               
X        DS    XL100                                                            
*                                                                               
SVDCMKEY DS    XL32                KEY SAVEAREA                                 
*                                                                               
SVDCMELM DS    12XL128             COMMENT ELEMENT SAVEAREA                     
*                                                                               
                                                                                
       EJECT                                                                    
*                                                                               
* ***************************                                                   
* ON-SCREEN ACTIVITY DISPLAY                                                    
* ***************************                                                   
DSPACTVD DSECT                                                                  
DSPUSR   DS    CL10                USERID                                       
         DS    CL1                                                              
DSPPID   DS    CL30                PERSON                                       
         DS    CL1                                                              
DSPDTE   DS    CL8                 DATE                                         
         DS    CL1                                                              
DSPDATA  DS    CL27                DATA CHANGED                                 
         EJECT                                                                  
*                                                                               
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
LISTD    DSECT                                                                  
LISTCODE DS    CL10                                                             
         DS    CL3                                                              
LISTMEDS DS    CL14                MEDIA CODES                                  
         DS    CL2                                                              
LISTCTDT DS    CL8                 CUT OFF DATE                                 
         DS    CL1                                                              
         DS    CL2                                                              
         EJECT                                                                  
                                                                                
*DDSPOOLD                                                                       
*DDCOMFACS                                                                      
*DDFLDIND                                                                       
*FAGETTXTD                                                                      
*PRGENFILE                                                                      
*PPERREQUS                                                                      
*CTGENFILE                                                                      
*SEACSFILE                                                                      
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE PRGENFILE                                                      
       ++INCLUDE PPERREQUS                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE FAFACTS                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067PRSFM39   07/17/18'                                      
         END                                                                    
