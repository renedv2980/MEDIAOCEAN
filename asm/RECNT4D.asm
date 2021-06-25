*          DATA SET RECNT4D    AT LEVEL 036 AS OF 05/16/12                      
*PHASE T8024DA,+0                                                               
*INCLUDE SQUASHER                                                               
*        TITLE 'T8024D - REPPAK AUDIT COMMENTS'                                 
         TITLE 'T8024D - REPPAK AUDIT COMMENTS'                                 
*                                                                               
*******************************************************************             
*                                                                 *             
*     RECNT4D (T8024D) --- AUDIT COMMENTS                         *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 12DEC11 BOB BIG BANG                                            *             
*                                                                 *             
*******************************************************************             
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - INIT'                          
*******************************************************************             
*                                                                 *             
*     INITIALISATION                                              *             
*                                                                 *             
*******************************************************************             
*                                                                               
T8024D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8024D,R9,RR=R2                                                
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC           ESTABLISH WORKING STORAGE                    
         USING TWAD,RA             ESTABLISH TWA                                
*                                                                               
         LR    R8,RA               ESTABLISH TWA WORK                           
         AH    R8,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R8                                                       
*                                                                               
         ST    R2,TWAREL4D         SAVE RELOCATION FACTOR                       
*                                                                               
MAINLINE EQU   *                                                                
*                                                                               
         LLC   RF,*+14                                                          
         SHI   RF,1                                                             
         BNZ   *+8                                                              
         DC    H'0'                                                             
         DC    X'01'                                                            
         DC    X'00'                                                            
         STC   RF,*-2                                                           
*                                                                               
         CLC   =C'AUL ',CONACT     IF LIST SCREEN                               
         BE    *+10                                                             
         CLC   =C'AULR',CONACT                                                  
         BE    *+10                                                             
         CLC   =C'AULS',CONACT                                                  
         BNE   MAINLRN                                                          
*                                                                               
*        CHECK IF PFKEY HIT                                                     
*                                                                               
         L     R1,AFACILS                                                       
         L     RF,0(R1)                                                         
         USING TIOBD,RF                                                         
*                                                                               
         LLC   R0,TIOBAID          GET PFKEY                                    
*                                                                               
         CHI   R0,12               KEYS ARE 1 THRU 12                           
         BNH   *+8                                                              
         SHI   R0,12                                                            
*                                                                               
         CHI   R0,9                IF CALL FOR A REPORT                         
         BNE   *+12                                                             
         BRAS  RE,PR                  PRINT REPORT                              
         B     MAINX                  DONE                                      
*                                                                               
         BRAS  RE,LR               GO HANDLE LIST SCREEN                        
*                                                                               
         B     MAINX                                                            
*                                                                               
MAINLRN  DS    0H                                                               
*                                                                               
         CLC   =C'AUD ',CONACT     IF DISPLAY SCREEN                            
         BNE   MAINDRN                                                          
*                                                                               
         OC    SVCONNUM,SVCONNUM   SKIP IF FIRST TIME TO SCREEN                 
         BZ    *+10                                                             
         CLC   SVCONNUM,RCONKCON   RESET ON CHANGE IN CONTRACT                  
         BE    *+22                                                             
         MVC   SVCONNUM,RCONKCON   SAVE NEW CONTRACT NUMBER                     
         XC    TWAMKGDS,TWAMKGDS   NO CURRENT  RECORD ON SCREEN                 
         XC    TWAMKGD2,TWAMKGD2   NO SELECTED RECORD ON SCREEN                 
*                                                                               
         BRAS  RE,VR                  VALIDATE SCREEN IF POSSIBLE               
         BE    MAINX                  VALIDATION OKAY                           
*                                                                               
         BRAS  RE,DR                  DISPLAY COMMENTS                          
*                                                                               
         B     MAINX                                                            
*                                                                               
MAINDRN  DS    0H                                                               
*                                                                               
MAIN0900 EQU   *                                                                
MAINX    DS    0H                                                               
         LA    R2,CONCACTH         SET BUY ACTION PREVALID                      
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'40'         SET CURSOR RETURN ADDRESS                    
         OI    1(R2),X'01'         TURN ON MODIFIED FOR AUTO PAGING             
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
*                                                                               
         LA    R2,CONCNUMH         SET CONTRACT NUMBER PREVALID                 
         OI    4(R2),X'20'                                                      
*                                                                               
         J     EXXMOD                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - DR'                            
*******************************************************************             
*                                                                 *             
*        DISPLAY AUDIT COMMENTS IN RECORD                         *             
*                                                                 *             
*******************************************************************             
*                                                                               
         DS    0D                                                               
DR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        READ LATEST AUDIT RECORD ON FILE                                       
*                                                                               
         XC    TWAMKGDH,TWAMKGDH   INIT DA OF LATEST AUDIT RECORD               
*                                                                               
         XC    KEY,KEY             INIT STARTING KEY                            
         XC    RAUDKEY(L'RAUDKEY),RAUDKEY  INIT STARTINF KEY                    
*                                                                               
         MVI   RAUDKTYP,X'4D'      SET RECORD CODE                              
         MVC   RAUDKREP,RCONKREP   SET REP CODE                                 
         MVC   RAUDKCON,RCONKCON   SET CONTRACT NUMBER                          
*                                                                               
         MVC   KEY(L'RAUDKEY),RAUDREC SET KEY FOR READ                          
*                                                                               
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH               READ FIRST AUDIT COMMENT RECORD              
*                                                                               
         CLC   KEY(RAUDKVER-RAUDKEY),KEYSAVE IF AUDIT REC FOR CON FOUND         
         BNE   *+10                                                             
         MVC   TWAMKGDH,KEY+28                  SAVE DA                         
*                                                                               
*        READ IN CURRENT RECORD                                                 
*                                                                               
         OC    TWAMKGD2,TWAMKGD2   IF SELECTED DA GIVEN                         
         BZ    *+10                                                             
         MVC   TWAMKGDS,TWAMKGD2      MAKE CURRENT DA                           
*                                                                               
         OC    TWAMKGDS,TWAMKGDS   IF CURRENT  DA GIVEN                         
         BZ    DRDAN                                                            
*                                                                               
*        CHECK IF PFKEY HIT                                                     
*                                                                               
         L     R1,AFACILS                                                       
         L     RF,0(R1)                                                         
         USING TIOBD,RF                                                         
*                                                                               
         LLC   R0,TIOBAID          GET PFKEY                                    
*                                                                               
         CHI   R0,12               KEYS ARE 1 THRU 12                           
         BNH   *+8                                                              
         SHI   R0,12                                                            
*                                                                               
         STC   R0,HALF             SAVE PF KEY HIT                              
*                                                                               
*        READ IN CURRENT AUDIT RECORD                                           
*                                                                               
         XC    KEY,KEY                INIT KEY AREA                             
         MVC   KEY+28(4),TWAMKGDS     DA OF CURRENT RECORD                      
*                                                                               
         GOTO1 VGETREC,DMCB,RAUDREC   READ IN AUDIT COMMENT RECORD              
*                                                                               
         MVC   KEY(27),RAUDKEY        COPY KEY                                  
         MVI   UPDATE,C'N'            SET UPDATE TO NO                          
         GOTO1 VHIGH                  SET POINTERS TO KEY                       
*                                                                               
*        PF8 MEANS NEXT AUDIT RECORD                                            
*                                                                               
         CLI   HALF,8              IF PF8 HIT                                   
         BNE   DRDAX                                                            
*                                                                               
         GOTO1 VSEQ                   READ NEXT RECORD ON FILE                  
*                                                                               
         CLC   RAUDKEY(RAUDKVER-RAUDKEY),KEY   OKAY IF SAME CONTRACT            
         BNE   DRDAN                  ON KEY BREAK START OVER                   
*                                                                               
         MVC   TWAMKGDS,KEY+28     MAKE CURRENT DA                              
*                                                                               
         GOTO1 VGETREC,DMCB,RAUDREC   READ IN AUDIT COMMENT RECORD              
*                                                                               
DRDAX    DS    0H                                                               
*                                                                               
         B     DRDR10                                                           
*                                                                               
DRDAN    DS    0H                  DISPLAY FIRST RECORD ON FILE                 
*                                                                               
         MVC   TWAMKGDS,TWAMKGDH   DEFAULT TO LATEST AUDIT RECORD               
         MVC   TWAMKGD2,TWAMKGDH   DEFAULT TO LATEST AUDIT RECORD               
*                                                                               
         OC    TWAMKGDS,TWAMKGDS   ERROR IF NO RECORDS                          
         BZ    DRERROR                                                          
*                                                                               
         XC    KEY,KEY                INIT KEY AREA                             
         MVC   KEY+28(4),TWAMKGDS     DA OF LATEST RECORD                       
*                                                                               
         GOTO1 VGETREC,DMCB,RAUDREC   READ IN AUDIT COMMENT RECORD              
*                                                                               
         MVC   KEY(L'RAUDKEY),RAUDREC SET KEY FOR READ                          
*                                                                               
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH               READ FIRST AUDIT COMMENT RECORD              
*                                                                               
DRDR10   DS    0H                                                               
*                                                                               
         MVC   TWAMKGDS,KEY+28     SAVE DA OF DISPLAYED RECORD                  
         MVC   TWAMKGD2,KEY+28     TREAT AS SELECTED                            
*                                                                               
*        CHECK IF PFKEY HIT                                                     
*                                                                               
         L     R1,AFACILS                                                       
         L     RF,0(R1)                                                         
         USING TIOBD,RF                                                         
*                                                                               
         LLC   R0,TIOBAID          GET PFKEY                                    
*                                                                               
         CHI   R0,12               KEYS ARE 1 THRU 12                           
         BNH   *+8                                                              
         SHI   R0,12                                                            
*                                                                               
         CHI   R0,9                IF CALL FOR A REPORT                         
         BNE   DRPRN                                                            
*                                                                               
         BRAS  RE,PRINIT              OPEN PRINT QUEUE                          
*                                                                               
         L     R7,ASPOOLD             ESTABLISH SPOOL WORKAREA                  
         USING SPOOLD,R7                                                        
*                                                                               
         BRAS  RE,PRLINE              PRINT REPORT                              
         BRAS  RE,PRCLOSE             CLOSE PRINT QUEUE                         
*                                                                               
         B     DRX                    DONE                                      
*                                                                               
DRPRN    DS    0H                                                               
*                                                                               
*        DISPLAY CONTRACT HEADLINE FIELDS                                       
*                                                                               
         BRAS  RE,DRCON                                                         
         BNE   DRERROR             EXIT ON ERRORS                               
*                                                                               
*        DISPLAY COMMENT LINES                                                  
*                                                                               
         MVC   AUDRCTT,=CL8'Rep Cmt'   SET TITLE                                
*                                                                               
         LA    R5,WORK3            ESTABLISH SAVED HEADER ELEMENT               
         USING RAUDTHEL,R5                                                      
*                                                                               
         CLI   RAUDTHTP,C'S'       IF STATION TYPE VERSION                      
         BNE   *+10                                                             
         MVC   AUDRCTT,=CL8'Sta Cmt'   SET TITLE                                
*                                                                               
         FOUT  AUDRCTTH            RE-DISPLAY FIELD                             
*                                                                               
         BRAS  RE,CLRAUD           CLEAR SCREEN                                 
*                                                                               
         LA    R2,AUDRCMTH         FIRST COMMENT LINE                           
         LA    R3,AUDCMTH          END OF COMMENT DISPLAY                       
*                                                                               
         LA    R6,RAUDREC          FIND AUDIT COMMENTS                          
         MVI   ELCODE,RAUDTXQ      ELEMENT ID                                   
         BRAS  RE,GETEL            FIND FIRST                                   
         BNE   DRACMDN             NONE FOUND                                   
*                                                                               
DRACMLP  DS    0H                                                               
*                                                                               
         BNE   DRACMDN             NONE FOUND                                   
*                                                                               
         USING RAUDTXEL,R6         ESTABLISH AUDIT TEXT ELEMENT                 
*                                                                               
         CR    R2,R3               DONE IF END OF COMMENT LNS REACHED           
         BNL   DRACMDN                                                          
*                                                                               
         LLC   RF,RAUDTXLN         ELEMENT LENGTH                               
         SHI   RF,RAUDTXHL         DECREMENT BY ELM HEADER LENGTH               
         BZ    DRACMCN             SKIP IF NO COMMENT                           
*                                                                               
         CHI   RF,L'AUDRCMT        MAKE SURE IT FITS ON SCREEN                  
         BNH   *+8                                                              
         LHI   RF,L'AUDRCMT                                                     
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RAUDTEXT    DISPLAY COMMENT                              
*                                                                               
         FOUT  (R2)                DISPLAY FIELD                                
*                                                                               
DRACMCN  DS    0H                                                               
*                                                                               
         LLC   RF,0(R2)            GET FIELD LENGTH                             
         LA    R2,0(RF,R2)         NEXT FIELD                                   
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT COMMENT ELEMENT                    
*                                                                               
         B     DRACMLP             PROCESS NEXT ELEMENT                         
*                                                                               
DRACMDN  DS    0H                                                               
*                                                                               
*        STATION COMMENTS                                                       
*                                                                               
         LA    R5,WORK3            ESTABLISH SAVED HEADER ELEMENT               
         USING RAUDTHEL,R5                                                      
*                                                                               
         CLI   TWAACCS,C'$'        IF STATION SIGNED ON                         
         BE    *+8                                                              
         CLI   RAUDTHTP,C'S'       OR STATION TYPE VERSION                      
         BNE   DRBCM10                                                          
*                                                                               
         OI    AUDCMTTH+1,X'0C'       HIDE BUYER'S COMMENTS TITLE               
         FOUT  AUDCMTTH               RE-DISPLAY FIELD                          
*                                                                               
         LA    R2,AUDCMTH          FIRST BUYER'S COMMENT LINE                   
         LA    R3,AUDTABH          END OF BUYER'S COMMENTS DISPLAY              
*                                                                               
DRBCM1LP DS    0H                                                               
*                                                                               
         CR    R2,R3               DONE AT END OF BUYER'S COMMENTS              
         BNL   DRBCM1DN                                                         
*                                                                               
         OI    1(R2),X'20'+X'0C'   PROTECT AND HIDE FIELD                       
*                                                                               
         FOUT  (R2)                                                             
*                                                                               
DRBCM1CN DS    0H                                                               
*                                                                               
         LLC   RF,0(R2)            GET FIELD LENGTH                             
         LA    R2,0(RF,R2)         BUMP TO NEXT FIELD                           
         B     DRBCM1LP                                                         
*                                                                               
DRBCM1DN DS    0H                                                               
*                                                                               
         B     DRBCMX                                                           
*                                                                               
DRBCM10  DS    0H                                                               
*                                                                               
         NI    AUDCMTTH+1,X'FF'-X'0C' SHOW BUYER'S COMMENTS TITLE               
         FOUT  AUDCMTTH               RE-DISPLAY FIELD                          
*                                                                               
         LA    R2,AUDCMTH          FIRST BUYER'S COMMENT LINE                   
         LA    R3,AUDTABH          END OF BUYER'S COMMENTS DISPLAY              
*                                                                               
         LA    R6,RAUDREC          FIND AUDIT COMMENTS                          
         MVI   ELCODE,RAUDCMQ      ELEMENT ID                                   
         BRAS  RE,GETEL            FIND FIRST                                   
*                                                                               
DRBCM2LP DS    0H                                                               
*                                                                               
         BNE   DRBCM2DN            NONE FOUND                                   
*                                                                               
         USING RAUDCMEL,R6         ESTABLISH AUDIT TEXT ELEMENT                 
*                                                                               
         CR    R2,R3               DONE IF END OF COMMENT LNS REACHED           
         BNL   DRBCM2DN                                                         
*                                                                               
         LLC   RF,RAUDCMLN         ELEMENT LENGTH                               
         SHI   RF,RAUDCMHL         DECREMENT BY ELM HEADER LENGTH               
         BZ    DRBCM2CN            SKIP IF NO COMMENT                           
*                                                                               
         CHI   RF,L'AUDCMT         MAKE SURE IT FITS ON SCREEN                  
         BNH   *+8                                                              
         LHI   RF,L'AUDCMT                                                      
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RAUDCMNT    DISPLAY COMMENT                              
*                                                                               
         NI    1(R2),X'FF'-X'20'   DISABLE PROTECTION                           
*                                                                               
         CLC   TWAMKGDS,SVREPDA    IF NOT DISPLAYING LATEST                     
         BE    *+8                                                              
         OI    1(R2),X'20'            PROTECT FIELD                             
*                                                                               
         NI    1(R2),X'FF'-X'0C'   SHOW FIELD                                   
*                                                                               
         FOUT  (R2)                DISPLAY FIELD                                
*                                                                               
DRBCM2CN DS    0H                                                               
*                                                                               
         LLC   RF,0(R2)            GET FIELD LENGTH                             
         LA    R2,0(RF,R2)         NEXT FIELD                                   
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT COMMENT ELEMENT                    
*                                                                               
         B     DRBCM2LP            PROCESS NEXT ELEMENT                         
*                                                                               
DRBCM2DN DS    0H                                                               
*                                                                               
*        FINISH PROTECTING/OPENING NOTES FIELDS                                 
*                                                                               
DRBCM3LP DS    0H                                                               
*                                                                               
         CR    R2,R3               DONE IF END OF COMMENT LNS REACHED           
         BNL   DRBCM3DN                                                         
*                                                                               
         NI    1(R2),X'FF'-X'20'   DISABLE PROTECTION                           
*                                                                               
         CLC   TWAMKGDS,SVREPDA    IF NOT DISPLAYING LATEST                     
         BE    *+8                                                              
         OI    1(R2),X'20'            PROTECT FIELD                             
*                                                                               
         NI    1(R2),X'FF'-X'0C'   SHOW FIELD                                   
*                                                                               
         FOUT  (R2)                DISPLAY FIELD                                
*                                                                               
DRBCM3CN DS    0H                                                               
*                                                                               
         LLC   RF,0(R2)            GET FIELD LENGTH                             
         LA    R2,0(RF,R2)         NEXT FIELD                                   
*                                                                               
         B     DRBCM3LP            PROCESS NEXT ELEMENT                         
*                                                                               
DRBCM3DN DS    0H                                                               
*                                                                               
DRBCMX   DS    0H                                                               
*                                                                               
         LA    RF,211              AUDIT COMMENTS DISPLAED MSG                  
         STCM  RF,3,DUB                                                         
*                                                                               
         B     DRX                                                              
*                                                                               
DRERROR  DS    0H                                                               
*                                                                               
DRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R5,R6                                                            
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - VR'                            
*******************************************************************             
*                                                                 *             
*        ADD/CHANGE NOTES AT BOTTOM OF SCREEN                     *             
*              COMMENTS MUST ALREADY BE DISPLAYED ON SCREEN       *             
*              CAN ONLY UPDATE NOTES FOR LATEST REP COMMENTS      *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
         DS    0D                                                               
VR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   UPDOK,0             INIT UPDATE SWITCH                           
*                                                                               
*        FIND LATEST REP COMMENTS ON FILE                                       
*                                                                               
         XC    KEY,KEY             INIT STARTING KEY                            
*                                                                               
         MVI   RAUDKTYP,X'4D'      SET RECORD CODE                              
         MVC   RAUDKREP,RCONKREP   SET REP CODE                                 
         MVC   RAUDKCON,RCONKCON   SET CONTRACT NUMBER                          
*                                                                               
         MVC   KEY(L'RAUDKEY),RAUDREC SET KEY FOR READ                          
*                                                                               
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH               READ FIRST AUDIT COMMENT RECORD              
*                                                                               
VRREPLP  DS    0H                                                               
*                                                                               
         CLC   KEY(RAUDKVER-RAUDKEY),KEYSAVE MUST BE SAME CONTRACT              
         BNE   VRNO                                                             
*                                                                               
         GOTO1 VGETREC,DMCB,RAUDREC   READ IN AUDIT COMMENT RECORD              
*                                                                               
*        LOOKING FOR LATEST REP RECORD                                          
*                                                                               
         LA    R5,RAUDTHEL         ESTABLISH HEADER ELEMENT                     
         USING RAUDTHEL,R5                                                      
*                                                                               
         CLI   RAUDTHTP,C'R'       TEST IF REP CREATED RECORD                   
         BE    VRREPFD                                                          
*                                                                               
VRREPCN  DS    0H                                                               
*                                                                               
         GOTO1 VSEQ                READ NEXT KEY ON FILE                        
*                                                                               
         B     VRREPLP                                                          
*                                                                               
VRREPFD  DS    0H                                                               
*                                                                               
         MVC   SVREPDA,KEY+28      SAVE LATEST REP DA                           
*                                                                               
         CLC   TWAMKGD2,KEY+28     SKIP IF NOT COMMENTS TO BE DISPLAYED         
         BE    *+10                                                             
         CLC   TWAMKGDS,KEY+28     OR   IF NOT COMMENTS ON SCREEN               
         BNE   VRNO                                                             
*                                                                               
         MVI   UPDOK,X'FF'         OKAY TO UPDATE BUYER'S NOTES                 
*                                                                               
         OC    TWAMKGDS,TWAMKGDS   SKIP IF NO COMMENTS ON SCREEN                
         BZ    VRNO                                                             
*                                                                               
         MVI   UPDATE,C'Y'         READ FOR UPDATE                              
*                                                                               
         GOTO1 VGETREC,DMCB,RAUDREC   READ IN AUDIT COMMENT RECORD              
*                                                                               
*        COMPARE NOTE LINES FOR CHANGES                                         
*                                                                               
         LA    R5,RAUDTHEL         ESTABLISH HEADER ELEMENT                     
         USING RAUDTHEL,R5                                                      
*                                                                               
         CLI   RAUDTHTP,C'S'       SKIP IF STATION TYPE VERSION                 
         BE    VRNO                                                             
*                                                                               
         LA    R2,AUDCMTH          FIRST COMMENT LINE                           
         LA    R3,AUDTABH          END OF COMMENT DISPLAY                       
*                                                                               
         LA    R6,RAUDREC          FIND AUDIT COMMENTS                          
         MVI   ELCODE,RAUDCMQ      ELEMENT ID                                   
         BRAS  RE,GETEL            FIND FIRST                                   
*                                                                               
VRACMLP  DS    0H                                                               
*                                                                               
         BNE   VRACMDN             NONE FOUND                                   
*                                                                               
         USING RAUDCMEL,R6         ESTABLISH AUDIT NOTE ELEMENT                 
*                                                                               
         CR    R2,R3               DONE IF END OF COMMENT LNS REACHED           
         BNL   VRACMDN                                                          
*                                                                               
         LLC   RF,RAUDCMLN         ELEMENT LENGTH                               
         SHI   RF,RAUDCMHL         DECREMENT BY ELM HEADER LENGTH               
         BNZ   VRACM10                SOME NOTE AVAILABLE                       
*                                                                               
         LAY   R1,MYSPACES                                                      
         CLC   8(L'AUDCMT,R2),0(R1)     SCREEN MUST BE EMPTY                    
         BH    VRACMFD                  SCREEN HAS DATA                         
*                                                                               
         B     VRACMCN                                                          
*                                                                               
VRACM10  DS    0H                                                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),RAUDCMNT    TEST FOR CHANGES TO NOTES                    
*                                                                               
         BNE   VRACMFD                CHANGES HAVE BEEN MADE                    
*                                                                               
VRACMCN  DS    0H                                                               
*                                                                               
         LLC   RF,0(R2)            GET FIELD LENGTH                             
         LA    R2,0(RF,R2)         NEXT FIELD                                   
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT COMMENT ELEMENT                    
*                                                                               
         B     VRACMLP             PROCESS NEXT ELEMENT                         
*                                                                               
VRACMDN  DS    0H                                                               
*                                                                               
*        MAKE SURE THERE ARE NO NEW NOTES                                       
*                                                                               
VRACM1LP DS    0H                                                               
*                                                                               
         CR    R2,R3               NO VALIDATION IF END OF NOTES                
         BNL   VRACM1DN                                                         
*                                                                               
         CLI   5(R2),0             VALIDATE IF NEW NOTE FOUND                   
         BNE   VRACM1FD                                                         
*                                                                               
VRACM1CN DS    0H                                                               
*                                                                               
         LLC   RF,0(R2)            GET FIELD LENGTH                             
         LA    R2,0(RF,R2)         NEXT FIELD                                   
*                                                                               
         B     VRACM1LP            PROCESS NEXT ELEMENT                         
*                                                                               
VRACM1DN DS    0H                                                               
*                                                                               
         B     VRNO                NO CHANGES TO NOTES                          
*                                                                               
VRACM1FD DS    0H                                                               
*                                                                               
VRACMFD  DS    0H                                                               
*                                                                               
*        REP NOTES VALIDATION                                                   
*                                                                               
*              FIRST DELETE CURRENT NOTE ELEMENTS                               
*                                                                               
         GOTO1 VDELELEM,DMCB,('RAUDCHQ',RAUDREC) DELETE NOTE HEADER ELM         
*                                                                               
         GOTO1 VDELELEM,DMCB,('RAUDCMQ',RAUDREC) DELETE NOTES ELEMENTS          
*                                                                               
*        BUILD HEADER ELEMENT                                                   
*                                                                               
         XC    WORK2,WORK2         ESTABLISH NOTES HDR ELM                      
         LA    R6,WORK2                                                         
         USING RAUDCHEL,R6                                                      
*                                                                               
         MVI   RAUDCHEL,RAUDCHQ    ELEMENT ID                                   
         MVI   RAUDCHLN,RAUDCHL    ELEMENT LENGTH                               
*                                                                               
         GOTO1 VADDELEM,DMCB,RAUDREC,RAUDCHEL ADD ELEMENT                       
*                                                                               
*        ADD LINES OF NOTES                                                     
*                                                                               
         LA    R2,AUDCMTH         FIRST BUYER'S COMMENT LINE                    
         LA    R3,AUDTABH          END OF BUYER'S COMMENTS DISPLAY              
         LA    R0,1                STARTING SEQ NUMBER                          
*                                                                               
VRBCM1LP DS    0H                                                               
*                                                                               
         CR    R2,R3               DONE AT END OF BUYER'S COMMENTS              
         BNL   VRBCM1DN                                                         
*                                                                               
         XC    WORK2,WORK2         ESTABLISH NOTES DETAIL ELM                   
         LA    R6,WORK2                                                         
         USING RAUDCMEL,R6                                                      
*                                                                               
         MVI   RAUDCMEL,RAUDCMQ    ELEMENT ID                                   
         MVI   RAUDCMLN,RAUDCMHL   ELEMENT LENGTH                               
*                                                                               
         LLC   RF,0(R2)            GET FIELD LENGTH                             
         SHI   RF,8                DECREMENT BY HEADER LENGTH                   
*                                                                               
         TM    1(R2),X'02'         IF TRAILING HEADER PRSENT                    
         BNO   *+8                                                              
         SHI   RF,8                   DECREMENT BY ITS LENGTH                   
*                                                                               
         LA    R1,8(RF,R2)         POINT TO END OF FIELD                        
         SHI   R1,1                                                             
         LR    RE,RF               MAX LENGTH OF FIELD                          
*                                  FIND END OF FIELD                            
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         SHI   R1,1                                                             
         BCT   RE,*-12                                                          
*                                                                               
         AHI   R1,1                POINT TO END OF FIELD                        
*                                                                               
         LA    RF,8(R2)            CALCULATE LENGTH OF FIELD                    
         SR    R1,RF                                                            
*                                                                               
         LTR   RF,R1                                                            
         BZ    VRBCM10             NO ENTRY IN FIELD                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RAUDCMNT(0),8(R2)   SAVE LINE OF NOTE                            
*                                                                               
         AHI   RF,1                RESTORE LENGTH                               
*                                                                               
VRBCM10  DS    0H                                                               
*                                                                               
         AHI   RF,RAUDCMHL         LENGTH OF ELEMENT                            
         STC   RF,RAUDCMLN                                                      
*                                                                               
         STC   R0,RAUDCMSQ         SET SEQUENCE NUMBER                          
*                                                                               
         GOTO1 VADDELEM,DMCB,RAUDREC,RAUDCMEL  ADD TO RECORD                    
*                                                                               
VRBCM1CN DS    0H                                                               
*                                                                               
         LLC   RF,0(R2)            GET FIELD LENGTH                             
         LA    R2,0(RF,R2)         BUMP TO NEXT FIELD                           
         AHI   R0,1                BUMP SEQUENCE NUMBER                         
         B     VRBCM1LP                                                         
*                                                                               
VRBCM1DN DS    0H                                                               
*                                                                               
VRBCMX   DS    0H                                                               
*                                                                               
         GOTO1 VPUTREC,DMCB,RAUDREC UPDATE FILE RECORD                          
*                                                                               
         LA    RF,210              AUDIT COMMENTS CHANGED MSG                   
         STCM  RF,3,DUB                                                         
*                                                                               
         CR    RB,RB               SET EQ CC                                    
*                                                                               
         B     VRX                                                              
*                                                                               
VRNO     DS    0H                                                               
*                                                                               
         LTR   RB,RB               SET NEQ CC                                   
         B     VRX                                                              
*                                                                               
VRERROR  DS    0H                                                               
*                                                                               
VRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R5,R6                                                            
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - LR'                            
*******************************************************************             
*                                                                 *             
*        LIST AUDIT COMMENT RECORDS                               *             
*                                                                 *             
*******************************************************************             
*                                                                               
         DS    0D                                                               
LR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    TWAMKGLA,TWAMKGLA   SKIP IF DOING NEXT PAGE                      
         BNZ   LRHDX                  IE. LAST LINE ON SCREEN HAS DATA          
*                                                                               
*        DISPLAY CONTRACT HEADLINE FIELDS                                       
*                                                                               
         BRAS  RE,DISPCON                                                       
         BNE   LRERROR             EXIT ON ERRORS                               
*                                                                               
LRHDX    DS    0H                                                               
*                                                                               
         BRAS  RE,CLRSCRN          CLEAR THE SCREEN                             
*                                                                               
         OC    TWAMKGLA,TWAMKGLA   IF DOING NEXT PAGE                           
         BZ    LR1ST                                                            
*                                                                               
         XC    KEY,KEY                INIT KEY AREA                             
         MVC   KEY+28(4),TWAMKGLA     DA OF LAST ITEM ON SCREEN                 
*                                                                               
         GOTO1 VGETREC,DMCB,RAUDREC   READ IN AUDIT COMMENT RECORD              
*                                                                               
         MVC   KEY(27),RAUDKEY        COPY KEY                                  
         MVI   UPDATE,C'N'            SET UPDATE TO NO                          
         GOTO1 VHIGH,DMCB,KEY         SET POINTERS TO KEY                       
*                                                                               
         GOTO1 VSEQ                   POINT TO NEXT KEY ON FILE                 
*                                                                               
         B     LRSTART                                                          
*                                                                               
LR1ST    DS    0H                                                               
*                                                                               
         XC    KEY,KEY             INIT STARTING KEY                            
*                                                                               
         MVI   RAUDKTYP,X'4D'      SET RECORD CODE                              
         MVC   RAUDKREP,RCONKREP   SET REP CODE                                 
         MVC   RAUDKCON,RCONKCON   SET CONTRACT NUMBER                          
*                                                                               
         MVC   KEY(L'RAUDKEY),RAUDREC SET KEY FOR READ                          
*                                                                               
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH               READ FIRST AUDIT COMMENT RECORD              
*                                                                               
LRSTART  DS    0H                                                               
*                                                                               
         XC    TWAMKGLA,TWAMKGLA   INIT DA OF LAST LINE ON SCREEN               
*                                                                               
         LA    R3,TWAMKGDA         POINT TO FIRST OF SAVED DA'S                 
         XC    TWAMKGDA(TWAMKGLA-TWAMKGDA),TWAMKGDA  INIT LIST OF DA'S          
*                                                                               
         LA    R2,ADLCSELH         FIRST SELECT FIELD                           
         LA    R0,((ADLCLSTH-ADLCSELH)/(ADLCSE2H-ADLCSELH))+1  # LINES          
*                                                                               
LRLP     DS    0H                                                               
*                                                                               
         CLC   KEY(RAUDKVER-RAUDKEY),KEYSAVE    DONE IF NEW CONTRACT            
         BNE   LRDN                                                             
*                                                                               
         CLI   KEY+19,0            SKIP IF OLD KEY                              
         BE    LRCN                                                             
*                                                                               
         CLI   KEY+26,0            SKIP IF OLD KEY                              
         BE    LRCN                                                             
*                                                                               
         GOTO1 VGETREC,DMCB,RAUDREC READ IN AUDIT COMMENT RECORD                
*                                                                               
         CLC   =C'AUL ',CONACT     SKIP IF FULL LIST                            
         BE    LRFILTX                                                          
*                                                                               
*        FILTER ON REP/STA                                                      
*                                                                               
         MVI   ELCODE,RAUDTHQ      SET HEADER ELEMENT CODE                      
         LA    R6,RAUDREC          SEARCH AUDIT COMMENT RECORD                  
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND ELEMENT                            
*                                                                               
         USING RAUDTHEL,R6         ESTABLISH HEADER ELEMENT                     
*                                                                               
         CLC   =C'AULR',CONACT     IF FILTERING ON REP COMMENTS                 
         BNE   LRREPX                                                           
*                                                                               
         CLI   RAUDTHTP,C'R'       DROP RECORD IF NOT FOR A REP                 
         BNE   LRCN                                                             
*                                                                               
         B     LRFD                ELSE KEEP                                    
*                                                                               
LRREPX   DS    0H                                                               
*                                                                               
         CLC   =C'AULS',CONACT     IF FILTERING ON STATION COMMENTS             
         BNE   LRSTAX                                                           
*                                                                               
         CLI   RAUDTHTP,C'S'       DROP REOCORD IF NOT FOR A STATION            
         BNE   LRCN                                                             
*                                                                               
         B     LRFD                ELSE KEEP                                    
*                                                                               
LRSTAX   DS    0H                                                               
*                                                                               
LRFILTX  DS    0H                                                               
*                                                                               
LRFD     DS    0H                  LIST RECORD ON SCREEN                        
*                                                                               
         MVC   0(4,R3),KEY+28      SAVE DA OF COMMENT                           
*                                                                               
         BRAS  RE,DISPLIN          DISPLAY COMMENT LINE                         
*                                                                               
         BCT   R0,*+14             CHECK FOR ROOM ON SCREEN                     
         MVC   TWAMKGLA,0(R3)        NO - SAVE DA OF LAST LINE                  
         B     LRDN                       END OF SCREEN                         
*                                                                               
         LA    R3,4(R3)            BUMP TO NEXT DA SAVEAREA                     
         LA    R2,(ADLCSE2H-ADLCSELH)(R2)  BUMP TO NEXT SCREEN LINE             
*                                                                               
LRCN     DS    0H                                                               
*                                                                               
         GOTO1 VSEQ                READ NEXT AUDIT COMMENT KEY                  
*                                                                               
         B     LRLP                                                             
*                                                                               
LRDN     DS    0H                                                               
*                                                                               
         LA    R2,CONCACTH         SET ACTION PREVALID                          
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'40'         SET CURSOR RETURN ADDRESS                    
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
*                                                                               
         LA    RF,209              SET RETURN MESSAGE                           
         STCM  RF,3,DUB            AUDIT COMMENTS LISTED                        
*                                                                               
         B     LRX                                                              
*                                                                               
LRERROR  DS    0H                                                               
*                                                                               
LRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
NEXTSELF EQU   ADLCSE2H-ADLCSELH   TO BUMP OVER ONE SCREEN LINE                 
NEXTSEL2 EQU   (ADLCSE2H-ADLCSELH)*2                                            
*                                  TO BUMP OVER TWO SCREEN LINES                
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - DISPCON'                       
***********************************************************************         
*                                                                     *         
*   DISPCON:   DISPLAY CONTRACT PORTION OF SCREEN FROM CONTRACT       *         
*        RECORD.  THIS CODE HAS BEEN LIFTED FROM THE RECNT20 MOD.     *         
*                                                                     *         
***********************************************************************         
*                                                                               
         DS    0D                  ALIGNMENT                                    
DISPCON  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RF,CONCNUMH         SET CONTRACT NUMBER TO PREVALID              
         OI    4(RF),X'20'                                                      
*                                                                               
*              BUILD AGENCY KEY                                                 
*                                                                               
         XC    IOAREA(32),IOAREA   INIT KEY                                     
*                                                                               
         MVI   RAGYKTYP,X'0A'      AGENCY REC TYPE                              
         MVC   RAGYKAGY,RCONKAGY   AGY                                          
         MVC   RAGYKAOF,RCONKAOF   OFFICE                                       
         MVC   RAGYKREP,REPALPHA   REP                                          
*                                                                               
         MVC   KEY,IOAREA          SET KEY                                      
         MVC   RAGYNAM1,WAGYEXP    INIT AGENCY NAME                             
*                                                                               
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH               READ FOR AGENCY KEY                          
*                                                                               
         CLC   KEY(27),KEYSAVE     SKIP IF NOT FOUND                            
         BNE   DDIS0120                                                         
*                                                                               
DDIS0040 GOTO1 VGETREC,DMCB,IOAREA  READ IN AGENCY RECORD                       
*                                  SAVE AGENCY DATA                             
         MVC   TWAAGNM1,RAGYNAM1   SCREEN NAME                                  
         MVC   TWAAGNM2,RAGYNAM2   CONTRACT NAME                                
         MVC   TWAAGSTT,RAGYSTAT   STATE                                        
         MVC   TWAAGZIP,RAGYZIP    ZIO CODE                                     
         MVC   TWAAEASY,RAGYPRO2   PROFILE FOR EASYLINK AGY COPY                
         MVC   TWAARISK,RAGYRISK   AGENCY CREDIT RISK                           
         MVC   TWAALIAB,RAGYLIAB   AGENCY LIABILITY POSITION                    
*                                                                               
DDIS0060 MVC   ADLAGY(4),RAGYKAGY  DISPLAY AGENCY                               
*                                                                               
         LAY   R1,MYSPACES                                                      
         CLC   RAGYKAOF,0(R1)      OFFICE?                                      
         BE    DDIS0080                                                         
*                                                                               
         LA    RE,ADLAGY           YES - DISPLAY OFFICE                         
         MVI   ADLAGY+4,C' '                                                    
*                                                                               
         CLI   0(RE),C' '          FIND FIRST BLANK IN AGENCY FIELD             
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
*                                                                               
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),RAGYKAOF    AGENCY OFFICE                                
*                                                                               
DDIS0080 DS    0H                                                               
*                                                                               
         MVC   ADLAGYN,RAGYNAM1    AGENCY NAME                                  
*                                                                               
*                                                                               
DDIS0120 DS    0H                                                               
*                                                                               
*              ADVERTISER                                                       
*                                                                               
DDIS0180 XC    IOAREA(32),IOAREA   READ INTO IOAREA                             
*                                                                               
         MVI   RADVKTYP,8          ADVRT RECORD CODE                            
         MVC   RADVKADV,RCONKADV   ADVERTISER                                   
         MVC   RADVKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         MVC   RADVNAME,WADVEXP    PRE SET ADVERTISER NAME                      
*                                                                               
         OC    WADVEXP,WADVEXP     SKIP RECORD GET IF CODE KNOWN                
         BNZ   DDIS0220                                                         
*                                                                               
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DDIS0240                                                         
*                                                                               
DDIS0200 GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
DDIS0220 MVC   ADLADV(4),RADVKADV  DISPLAY ADVERTISER CODE                      
         MVC   ADLADVN,RADVNAME    NAME                                         
         EJECT                                                                  
*                                                                               
*              STATION                                                          
*                                                                               
DDIS0240 XC    IOAREA(32),IOAREA   BUILD STATION KEY                            
*                                                                               
         MVI   RSTAKTYP,2                                                       
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RCONKSTA                                                
         MVC   RSTAMKT,WSTAEXP                                                  
*                                                                               
         OC    WSTAEXP,WSTAEXP                                                  
         BNZ   DDIS0300                                                         
*                                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 VREAD                                                            
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
DDIS0300 DS    0H                                                               
*                                                                               
         MVC   ADLSTAM,RSTAMKT                                                  
*                                                                               
         MVC   WORK(4),RSTAKSTA                                                 
*                                                                               
         MVC   WORK+4(3),=C'-FM'                                                
*                                                                               
         CLI   RSTAKSTA+4,C'F'                                                  
         BE    DDIS0320                                                         
*                                                                               
         MVI   WORK+5,C'A'                                                      
         CLI   RSTAKSTA+4,C'A'                                                  
         BE    DDIS0320                                                         
*                                                                               
         MVI   WORK+5,C'C'                                                      
         CLI   RSTAKSTA+4,C'C'                                                  
         BE    DDIS0320                                                         
*                                                                               
         MVC   WORK+5(2),=C'L '                                                 
         CLI   RSTAKSTA+4,C'L'                                                  
         BE    DDIS0320                                                         
*                                                                               
         MVC   WORK+5(2),=C'TV'                                                 
*                                                                               
DDIS0320 CLI   WORK+3,C' '         ADJUST FOR 3 LETTER STATION CODE             
         BNE   *+14                                                             
         MVC   WORK+3(3),WORK+4                                                 
         MVI   WORK+6,C' '                                                      
*                                                                               
         MVC   ADLSTA(7),WORK      STATION                                      
*                                                                               
*        PRODUCT                                                                
*                                                                               
DDIS0380 LAY   R1,MYSPACES                                                      
         CLC   RCONPRD,0(R1)       PRODUCT CODE?                                
         BE    DDIS0440                                                         
*                                                                               
*        GET PRODUCT RECORD                                                     
*                                                                               
         XC    IOAREA(32),IOAREA                                                
*                                                                               
         MVI   RPRDKTYP,9                                                       
         MVC   RPRDKADV,RCONKADV                                                
         MVC   RPRDKPRD,RCONPRD                                                 
         MVC   RPRDKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
*                                                                               
         MVC   RPRDNAME,WPRDEXP                                                 
*                                                                               
         OC    WPRDEXP,WPRDEXP     PRODUCT LOOKED UP ALREADY?                   
         BZ    DDIS0400                                                         
*                                                                               
         MVC   ADLPRD(2),=C'C='                                                 
         MVC   ADLPRD+2(3),RCONPRD                                              
         MVI   ADLPRD+5,0                                                       
         MVC   ADLPRD+6(14),RPRDNAME                                            
*                                                                               
         B     DDIS0460                                                         
*                                                                               
DDIS0400 GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DDIS0460                                                         
*                                                                               
DDIS0420 GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         MVC   TWAPRDNM,RPRDNAME                                                
         MVC   ADLPRD(2),=C'C='                                                 
         MVC   ADLPRD+2(3),RCONPRD                                              
         MVI   ADLPRD+5,0                                                       
         MVC   ADLPRD+6(14),RPRDNAME                                            
*                                                                               
         B     DDIS0460                                                         
*                                                                               
*              FIND PRODUCT ELEMENT                                             
*                                                                               
DDIS0440 DS    0H                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO X'05'                                     
*                                                                               
         MVC   ADLPRD,2(R6)        PRODUCT EXPANSION                            
*                                                                               
         EJECT                                                                  
*                                                                               
*              GET SALESMAN                                                     
*                                                                               
DDIS0460 XC    IOAREA(32),IOAREA                                                
*                                                                               
         MVI   RSALKTYP,6          KEY TYPE - SALESMAN RECORD                   
         MVC   RSALKREP,REPALPHA   REP                                          
         MVC   RSALKSAL,RCONSAL    SALESMAN CODE                                
         MVC   KEY,IOAREA                                                       
         MVC   RSALNAME,WSALEXP                                                 
*                                                                               
         OC    WSALEXP,WSALEXP     SKIP IF SALESPERSON FOUND ALREADY            
         BNZ   DDIS0480                                                         
*                                                                               
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH               READ IN SALESPERSON RECORD                   
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DDIS0480                                                         
*                                                                               
         MVC   TWASALAS,22(R6)     SALES ASSISTANT                              
*                                                                               
DDIS0480 DC    0H'0'                                                            
*                                                                               
         MVC   ADLSAL(3),RSALKSAL  SALESMAN CODE                                
         MVC   ADLSALN,RSALNAME                                                 
*                                                                               
*              K START DATE                                                     
*                                                                               
DDIS0620 DS    0H                  DISPLAY CONTRACT DATES                       
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,(X'80',RCONREC),ADLODTS,0,DUB             
*                                                                               
* SET LENGTH OF DATES FOR RE-INPUT (ADDR)                                       
*                                                                               
         MVI   ADLODTSH+5,17                                                    
*                                                                               
         CR    RB,RB               FORCE EQ CC                                  
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - DRCON'                         
***********************************************************************         
*                                                                     *         
*   DRCON  :   DISPLAY CONTRACT PORTION OF SCREEN FROM CONTRACT       *         
*        RECORD.  THIS CODE HAS BEEN LIFTED FROM THE RECNT20 MOD.     *         
*                                                                     *         
***********************************************************************         
*                                                                               
         DS    0D                  ALIGNMENT                                    
DRCON    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RF,CONCNUMH         SET CONTRACT NUMBER TO PREVALID              
         OI    4(RF),X'20'                                                      
*                                                                               
*              BUILD AGENCY KEY                                                 
*                                                                               
         XC    IOAREA(32),IOAREA   INIT KEY                                     
*                                                                               
         MVI   RAGYKTYP,X'0A'      AGENCY REC TYPE                              
         MVC   RAGYKAGY,RCONKAGY   AGY                                          
         MVC   RAGYKAOF,RCONKAOF   OFFICE                                       
         MVC   RAGYKREP,REPALPHA   REP                                          
*                                                                               
         MVC   KEY,IOAREA          SET KEY                                      
         MVC   RAGYNAM1,WAGYEXP    INIT AGENCY NAME                             
*                                                                               
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH               READ FOR AGENCY KEY                          
*                                                                               
         CLC   KEY(27),KEYSAVE     SKIP IF NOT FOUND                            
         BNE   DRIS0120                                                         
*                                                                               
DRIS0040 GOTO1 VGETREC,DMCB,IOAREA  READ IN AGENCY RECORD                       
*                                  SAVE AGENCY DATA                             
         MVC   TWAAGNM1,RAGYNAM1   SCREEN NAME                                  
         MVC   TWAAGNM2,RAGYNAM2   CONTRACT NAME                                
         MVC   TWAAGSTT,RAGYSTAT   STATE                                        
         MVC   TWAAGZIP,RAGYZIP    ZIO CODE                                     
         MVC   TWAAEASY,RAGYPRO2   PROFILE FOR EASYLINK AGY COPY                
         MVC   TWAARISK,RAGYRISK   AGENCY CREDIT RISK                           
         MVC   TWAALIAB,RAGYLIAB   AGENCY LIABILITY POSITION                    
*                                                                               
DRIS0060 MVC   AUDAGY(4),RAGYKAGY  DISPLAY AGENCY                               
*                                                                               
         LAY   R1,MYSPACES                                                      
         CLC   RAGYKAOF,0(R1)      OFFICE?                                      
         BE    DRIS0080                                                         
*                                                                               
         LA    RE,AUDAGY           YES - DISPLAY OFFICE                         
         MVI   AUDAGY+4,C' '                                                    
*                                                                               
         CLI   0(RE),C' '          FIND FIRST BLANK IN AGENCY FIELD             
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
*                                                                               
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),RAGYKAOF    AGENCY OFFICE                                
*                                                                               
DRIS0080 DS    0H                                                               
*                                                                               
         MVC   AUDAGYN,RAGYNAM1    AGENCY NAME                                  
*                                                                               
DRIS0120 DS    0H                                                               
*                                                                               
*              ADVERTISER                                                       
*                                                                               
DRIS0180 XC    IOAREA(32),IOAREA   READ INTO IOAREA                             
*                                                                               
         MVI   RADVKTYP,8          ADVRT RECORD CODE                            
         MVC   RADVKADV,RCONKADV   ADVERTISER                                   
         MVC   RADVKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         MVC   RADVNAME,WADVEXP    PRE SET ADVERTISER NAME                      
*                                                                               
         OC    WADVEXP,WADVEXP     SKIP RECORD GET IF CODE KNOWN                
         BNZ   DRIS0220                                                         
*                                                                               
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DRIS0240                                                         
*                                                                               
DRIS0200 GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
DRIS0220 MVC   AUDADV(4),RADVKADV  DISPLAY ADVERTISER CODE                      
         MVC   AUDADVN,RADVNAME    NAME                                         
*                                                                               
         EJECT                                                                  
*                                                                               
*              STATION                                                          
*                                                                               
DRIS0240 XC    IOAREA(32),IOAREA   BUILD STATION KEY                            
*                                                                               
         MVI   RSTAKTYP,2                                                       
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RCONKSTA                                                
         MVC   RSTAMKT,WSTAEXP                                                  
*                                                                               
         OC    WSTAEXP,WSTAEXP                                                  
         BNZ   DRIS0300                                                         
*                                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 VREAD                                                            
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
DRIS0300 DS    0H                                                               
*                                                                               
         MVC   AUDSTAM,RSTAMKT                                                  
*                                                                               
         MVC   WORK(4),RSTAKSTA                                                 
*                                                                               
         MVC   WORK+4(3),=C'-FM'                                                
*                                                                               
         CLI   RSTAKSTA+4,C'F'                                                  
         BE    DRIS0320                                                         
*                                                                               
         MVI   WORK+5,C'A'                                                      
         CLI   RSTAKSTA+4,C'A'                                                  
         BE    DRIS0320                                                         
*                                                                               
         MVI   WORK+5,C'C'                                                      
         CLI   RSTAKSTA+4,C'C'                                                  
         BE    DRIS0320                                                         
*                                                                               
         MVC   WORK+5(2),=C'L '                                                 
         CLI   RSTAKSTA+4,C'L'                                                  
         BE    DRIS0320                                                         
*                                                                               
         MVC   WORK+5(2),=C'TV'                                                 
*                                                                               
DRIS0320 CLI   WORK+3,C' '         ADJUST FOR 3 LETTER STATION CODE             
         BNE   *+14                                                             
         MVC   WORK+3(3),WORK+4                                                 
         MVI   WORK+6,C' '                                                      
*                                                                               
         MVC   AUDSTA(7),WORK      STATION                                      
*                                                                               
*        PRODUCT                                                                
*                                                                               
DRIS0380 LAY   R1,MYSPACES                                                      
         CLC   RCONPRD,0(R1)       PRODUCT CODE?                                
         BE    DRIS0440                                                         
*                                                                               
*        GET PRODUCT RECORD                                                     
*                                                                               
         XC    IOAREA(32),IOAREA                                                
*                                                                               
         MVI   RPRDKTYP,9                                                       
         MVC   RPRDKADV,RCONKADV                                                
         MVC   RPRDKPRD,RCONPRD                                                 
         MVC   RPRDKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
*                                                                               
         MVC   RPRDNAME,WPRDEXP                                                 
*                                                                               
         OC    WPRDEXP,WPRDEXP     PRODUCT LOOKED UP ALREADY?                   
         BZ    DRIS0400                                                         
*                                                                               
         MVC   AUDPRD(2),=C'C='                                                 
         MVC   AUDPRD+2(3),RCONPRD                                              
         MVI   AUDPRD+5,0                                                       
         MVC   AUDPRD+6(14),RPRDNAME                                            
*                                                                               
         B     DRIS0460                                                         
*                                                                               
DRIS0400 GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DRIS0460                                                         
*                                                                               
DRIS0420 GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         MVC   TWAPRDNM,RPRDNAME                                                
         MVC   AUDPRD(2),=C'C='                                                 
         MVC   AUDPRD+2(3),RCONPRD                                              
         MVI   AUDPRD+5,0                                                       
         MVC   AUDPRD+6(14),RPRDNAME                                            
*                                                                               
         B     DRIS0460                                                         
*                                                                               
*              FIND PRODUCT ELEMENT                                             
*                                                                               
DRIS0440 DS    0H                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO X'05'                                     
*                                                                               
         MVC   AUDPRD,2(R6)        PRODUCT EXPANSION                            
*                                                                               
         EJECT                                                                  
*                                                                               
*              GET SALESMAN                                                     
*                                                                               
DRIS0460 XC    IOAREA(32),IOAREA                                                
*                                                                               
         MVI   RSALKTYP,6          KEY TYPE - SALESMAN RECORD                   
         MVC   RSALKREP,REPALPHA   REP                                          
         MVC   RSALKSAL,RCONSAL    SALESMAN CODE                                
         MVC   KEY,IOAREA                                                       
         MVC   RSALNAME,WSALEXP                                                 
*                                                                               
         OC    WSALEXP,WSALEXP     SKIP IF SALESPERSON FOUND ALREADY            
         BNZ   DRIS0480                                                         
*                                                                               
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH               READ IN SALESPERSON RECORD                   
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DRIS0480                                                         
*                                                                               
         MVC   TWASALAS,22(R6)     SALES ASSISTANT                              
*                                                                               
DRIS0480 DC    0H'0'                                                            
*                                                                               
         MVC   AUDSAL(3),RSALKSAL  SALESMAN CODE                                
         MVC   AUDSALN,RSALNAME                                                 
*                                                                               
*        CONTRACT DATES                                                         
*                                                                               
DRIS0620 DS    0H                  DISPLAY CONTRACT DATES                       
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,(X'80',RCONREC),AUDODTS,0,DUB             
*                                                                               
*        SET LENGTH OF DATES FOR RE-INPUT (ADDR)                                
*                                                                               
         MVI   AUDODTSH+5,17                                                    
*                                                                               
*        VERSION NUMBER                                                         
*                                                                               
         XC    AUDVER,AUDVER       CLEAR VERSION FIELD                          
*                                                                               
         MVI   ELCODE,RAUDTHQ      SET HEADER ELEMENT CODE                      
         LA    R6,RAUDREC          SEARCH AUDIT COMMENT RECORD                  
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND ELEMENT                            
*                                                                               
         USING RAUDTHEL,R6         ESTABLISH HEADER ELEMENT                     
*                                                                               
         MVC   WORK3,RAUDTHEL      SAVE HEADER ELEMENT                          
*                                                                               
         LA    R2,AUDVERH          POINT TO VERSION FIELD                       
         XC    AUDVER,AUDVER       INIT VERSION                                 
*                                                                               
         CLI   RAUDKMOD,X'FF'      SKIP IF MOD LEVEL GIVEN                      
         BNE   DRMOD                                                            
*                                                                               
         MVC   8(3,R2),=C'VER'      SET 'VR' TITLE                              
*                                                                               
         MVC   12(1,R2),RAUDTHTP   DISPLAY SENDER TYPE                          
*                                                                               
*        VERSION NUMBER LEFT JUSTIFIED                                          
*                                                                               
         MVC   FULL(1),RAUDKVER    COMPLEMENTED VERSION NUMBER                  
         XI    FULL,X'FF'          UMCOMPLEMENT VERSION NUMBER                  
*                                                                               
         LLC   RF,FULL                                                          
         CVD   RF,DUB              CVD                                          
*                                                                               
         MVC   FULL,=X'40202020'   EDIT PATTERN                                 
         ED    FULL(4),DUB+6                                                    
*                                                                               
         LHI   RF,3                MAX VERSION DIGITS                           
         LA    R1,FULL+1                                                        
*                                                                               
         CLI   0(R1),C' '          LEFT JUSTIFY                                 
         BH    *+12                                                             
         AHI   R1,1                                                             
         BCT   RF,*-12                                                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R2),0(R1)                                                   
*                                                                               
         B     DRMODX                                                           
*                                                                               
DRMOD    DS    0H                                                               
*                                                                               
         LA    R4,8(R2)            START OF MOD NUMBER                          
*                                                                               
         MVC   FULL+1(1),RAUDKMOD  COMPLEMENTED MOD NUMBER                      
         XI    FULL+1,X'FF'        UNCOMPLEMENT                                 
*                                                                               
         LLC   RF,FULL+1           GET MOD NUMBER                               
         SHI   RF,1                DECREMENT BY ONE                             
         CVD   RF,DUB                                                           
*                                                                               
         MVC   FULL,=X'40202120'   EDIT PATTERN                                 
         ED    FULL(4),DUB+6                                                    
*                                                                               
         MVI   0(R4),C'M'                                                       
         LHI   RF,2                MAX MOD DIGITS                               
         LA    R1,FULL+2                                                        
*                                                                               
         CLI   0(R1),C' '          LEFT JUSTIFY                                 
         BH    *+12                                                             
         SHI   RF,1                                                             
         AHI   R1,1                                                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),0(R1)                                                    
*                                                                               
         LA    R4,2(RF,R4)         NEXT PRINT AREA                              
*                                                                               
         MVI   0(R4),C'/'          SEPARATOR                                    
         MVI   1(R4),C'V'          VERSION ID                                   
         LA    R4,2(R4)            NEXT PRINT AREA                              
*                                                                               
         MVC   FULL(1),RAUDKVER    COMPLEMENTED VERSION NUMBER                  
         XI    FULL,X'FF'          UMCOMPLEMENT VERSION NUMBER                  
*                                                                               
         LLC   RF,FULL                                                          
         CVD   RF,DUB              CVD                                          
*                                                                               
         MVC   FULL,=X'40202020'   EDIT PATTERN                                 
         ED    FULL(4),DUB+6                                                    
*                                                                               
         LHI   RF,3                MAX VERSION DIGITS                           
         LA    R1,FULL+1                                                        
*                                                                               
         CLI   0(R1),C' '          LEFT JUSTIFY                                 
         BH    *+12                                                             
         AHI   R1,1                                                             
         BCT   RF,*-12                                                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R1)                                                    
*                                                                               
DRMODX   DS    0H                                                               
*                                                                               
         FOUT  (R2)                DISPLAY FIELD                                
*                                                                               
         XC    AUDADT,AUDADT       INIT SENDING DATE                            
*                                                                               
         GOTO1 DATCON,DMCB,(2,RAUDTHDT),(5,AUDADT)   SENDING DATE               
*                                                                               
         FOUT  AUDADTH             DISPLAY FIELD                                
*                                                                               
         MVC   AUDATM(2),RAUDTHTM     DISPLAY TIME                              
         MVI   AUDATM+2,C':'                                                    
         MVC   AUDATM+3(2),RAUDTHTM+2                                           
         MVI   AUDATM+5,C':'                                                    
         MVC   AUDATM+6(2),RAUDTHTM+4                                           
*                                                                               
         FOUT  AUDATMH             DISPLAY FIELD                                
*                                                                               
*        PID DISPLAY                                                            
*                                                                               
         CLI   RAUDTHTP,C'S'       IF STATION COMMENTS                          
         BNE   DRPID10                                                          
*                                                                               
         OI    AUDPIDTH+1,X'0C'       SET TO NO TITLE DISPLAY                   
         FOUT  AUDPIDTH               DISPLAY TITLE FIELD                       
         OI    AUDPIDH+1,X'0C'        SET TO NO PID DISPLAY                     
         FOUT  AUDPIDH                DISPLAY PID FIELD                         
         B     DRPIDX                                                           
*                                                                               
DRPID10  DS    0H                                                               
*                                                                               
         NI    AUDPIDTH+1,X'FF'-X'0C' SET TO NORMAL TITLE DISPLAY               
         FOUT  AUDPIDTH               DISPLAY TITLE FIELD                       
*                                                                               
         NI    AUDPIDH+1,X'FF'-X'0C'  SET TO NORMAL PID DISPLAY                 
*                                                                               
         LA    R2,AUDPIDH          POINT TO PID FIELD                           
*                                                                               
         GOTOR TRNPID,DMCB,(R6),0   TRANSLATE PID                               
*                                                                               
         FOUT  AUDPIDH             DISPLAY FIELD                                
*                                                                               
DRPIDX   DS    0H                                                               
*                                                                               
*        SALES ASSISTANT DISPLAY                                                
*                                                                               
         CLI   RAUDTHTP,C'S'       IF STATION COMMENTS                          
         BNE   DRAST10                                                          
*                                                                               
         OI    AUDASTTH+1,X'0C'       SET TO NO TITLE DISPLAY                   
         FOUT  AUDASTTH               DISPLAY TITLE FIELD                       
*                                                                               
         OI    AUDASTH+1,X'0C'        SET TO NO AST DISPLAY                     
         FOUT  AUDASTH                DISPLAY AST FIELD                         
*                                                                               
         B     DRASTX                                                           
*                                                                               
DRAST10  DS    0H                                                               
*                                                                               
         NI    AUDASTTH+1,X'FF'-X'0C' SET TO NORMAL TITLE DISPLAY               
         FOUT  AUDASTTH               DISPLAY TITLE FIELD                       
*                                                                               
         NI    AUDASTH+1,X'FF'-X'0C'  SET TO NORMAL AST DISPLAY                 
*                                                                               
         LAY   R1,MYSPACES                                                      
         MVC   AUDAST,0(R1)                                                     
*                                                                               
         CLI   RAUDTHLN,RAUDTHL    SKIP IF SHORT ELEMENT                        
         BL    *+10                                                             
         MVC   AUDAST,RAUDTHAS        SALES ASSISTANT                           
*                                                                               
         FOUT  AUDASTH             DISPLAY FIELD                                
*                                                                               
DRASTX   DS    0H                                                               
*                                                                               
*        CONTRACT TOTAL                                                         
*                                                                               
         EDIT  (P8,RAUDTH$),AUDTOT,2,COMMAS=YES,FLOAT=-                         
*                                                                               
         FOUT  AUDTOTH             DISPLAY FIELD                                
*                                                                               
         CR    RB,RB               FORCE EQ CC                                  
*                                                                               
DRCONX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
*                                                                               
         TITLE 'T8024D - AUDIT COMMENTS MAINT/LIST - TRNPID'                    
***********************************************************************         
*                                                                     *         
*        TRANSLATE PID TO A NAME                                      *         
*                                                                     *         
*NTRY    R2 ==>   SCREEN FIELD                                        *         
*        P1 ==>   A(RAUDTHEL) - AUDIT COMMENTS HEADER ELEMENT         *         
*        P2+0(1)  L'RETURN AREA  IF R2 = 0                            *         
*        P2+1(3)  A(RETURN AREA) IF R2 = 0                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TRNPID   NTR1  BASE=*,LABEL=*,WORK=(R4,500)      TRANSLATE PID                  
*                                                                               
         L     R6,0(R1)            POINT TO AUDIT HEADER ELEMENT                
         USING RAUDTHEL,R6         ESTABLISH COMMENT HEADER ELEMENT             
*                                                                               
         SR    R5,R5                                                            
         IC    R5,4(R1)            SAVE LENGTH OF RETURN AREA                   
         L     R3,4(R1)            POINT TO RETURN AREA                         
*                                                                               
         LTR   R2,R2               IF R2 GIVEN                                  
         BZ    *+8                                                              
         LA    R3,8(R2)               USE DATAAREA OF SCREEN FIELD              
*                                                                               
         OC    RAUDTHPD,RAUDTHPD   SKIP IF NO PID FOUND                         
         BZ    TPIDNOTF                                                         
*                                                                               
*        READ PERSON AUTH REC ON CTFILE INTO LOCAL WORKAREA                     
*                                                                               
         USING CT0REC,R4           ESTABLISH KEY AS PERSON AUTH REC             
*                                                                               
         XC    CT0KEY,CT0KEY       INIT KEY                                     
*                                                                               
         MVI   CT0KTYP,CT0KTEQU    SET RECORD TYPE                              
         MVC   CT0KAGY,RAUDTHAG    SET SECURITY AGENCY                          
*                                                                               
         CLC   CT0KAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   CT0KAGY,RCONKREP       USE CONTRACT REP                          
*                                                                               
         MVC   CT0KNUM,RAUDTHPD    SET PID                                      
*                                                                               
         MVC   KEYSAVE,CT0KEY      SAVE STARTING KEY                            
*                                                                               
         GOTO1 DATAMGR,DMCB,=CL7'DMRDHI',=CL7'CTFILE',CT0KEY,CT0KEY,0,0         
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
*        RETURN PID FOUND                                                       
*                                                                               
         MVC   0(8,R3),2(RE)       RETURN PID                                   
*                                                                               
         B     TRNPIDX                                                          
*                                                                               
TPIDNOTF DS    0H                  PRINT 'UNKNOWN' IF NO PID                    
*                                                                               
         MVC   0(8,R3),=CL8'UNKNOWN'                                            
*                                                                               
TRNPIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - DISPLIN'                       
***********************************************************************         
*                                                                     *         
*   DISPLIN:   DISPLAY AUDIT COMMENT RECORD IN LIST                   *         
*                                                                     *         
*NTR     R2==> LINE ON SCREEN                                         *         
*        RAUDREC - CONTAINS RECORD FOR DISPLAY                        *         
*                                                                     *         
***********************************************************************         
*                                                                               
         DS    0D                  ALIGNMENT                                    
DISPLIN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        FIND HEADER ELEMENT                                                    
*                                                                               
         MVI   ELCODE,RAUDTHQ      SET HEADER ELEMENT CODE                      
         LA    R6,RAUDREC          SEARCH AUDIT COMMENT RECORD                  
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND ELEMENT                            
*                                                                               
         USING RAUDTHEL,R6         ESTABLISH HEADER ELEMENT                     
*                                                                               
         LLC   RF,0(R2)            LENGTH OF SELECT FIELD                       
         LA    R7,0(RF,R2)         POINT TO DESCRIPTION FIELD                   
*                                                                               
         LA    R3,8(R7)            POINT TO DESCRIPTION AREA                    
*                                                                               
         USING LISTD,R3            ESTABLISH DESCRIPTION FIELD                  
*                                                                               
         XC    LSLINE(LSLISTL),LSLINE INIT DISPLAY AREA                         
*                                                                               
         MVC   LSTYP,RAUDTHTP      DISPLAY SENDER TYPE                          
*                                                                               
*        MOD/VERSION NUMBER LEFT JUSTIFIED                                      
*                                                                               
         LA    R4,LSVER            VERSION PRINT AREA                           
*                                                                               
         CLI   RAUDKMOD,X'FF'      SKIP IF NO MOD NUMBER                        
         BE    DLNMODX                                                          
*                                                                               
         MVC   FULL+1(1),RAUDKMOD  COMPLEMENTED MOD NUMBER                      
         XI    FULL+1,X'FF'        UNCOMPLEMENT                                 
*                                                                               
         LLC   RF,FULL+1           GET MOD NUMBER                               
         SHI   RF,1                DECREMENT BY ONE                             
         CVD   RF,DUB                                                           
*                                                                               
         MVC   FULL,=X'40202120'   EDIT PATTERN                                 
         ED    FULL(4),DUB+6                                                    
*                                                                               
         MVI   0(R4),C'M'                                                       
         LHI   RF,2                MAX MOD DIGITS                               
         LA    R1,FULL+2                                                        
*                                                                               
         CLI   0(R1),C' '          LEFT JUSTIFY                                 
         BH    *+12                                                             
         SHI   RF,1                                                             
         AHI   R1,1                                                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),0(R1)                                                    
*                                                                               
         LA    R4,2(RF,R4)         NEXT PRINT AREA                              
*                                                                               
         MVI   0(R4),C'/'          SEPARATOR                                    
         MVI   1(R4),C'V'          VERSION ID                                   
         LA    R4,2(R4)            NEXT PRINT AREA                              
*                                                                               
DLNMODX  DS    0H                                                               
*                                                                               
         MVC   FULL(1),RAUDKVER    COMPLEMENTED VERSION NUMBER                  
         XI    FULL,X'FF'          UMCOMPLEMENT VERSION NUMBER                  
*                                                                               
         LLC   RF,FULL                                                          
         CVD   RF,DUB              CVD                                          
*                                                                               
         MVC   FULL,=X'40202020'   EDIT PATTERN                                 
         ED    FULL(4),DUB+6                                                    
*                                                                               
         LHI   RF,3                MAX VERSION DIGITS                           
         LA    R1,FULL+1                                                        
*                                                                               
         CLI   0(R1),C' '          LEFT JUSTIFY                                 
         BH    *+12                                                             
         AHI   R1,1                                                             
         BCT   RF,*-12                                                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R1)                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(2,RAUDTHDT),(17,LSDATE)   SENDING DATE              
*                                                                               
*        FIND FIRST TEXT ELEMENT                                                
*                                                                               
         XC    LSCMT,LSCMT         INIT COMMENT DISPLAY                         
*                                                                               
         MVI   ELCODE,RAUDTXQ      SET TEXT ELEMENT CODE                        
         LA    R6,RAUDREC          SEARCH AUDIT COMMENT RECORD                  
         BRAS  RE,GETEL                                                         
         BNE   DSPTXTX             NONE                                         
*                                                                               
         USING RAUDTXEL,R6         ESTABLISH TEXT ELEMENT                       
*                                                                               
         LLC   RF,RAUDTXLN         GET ELEMENT LENGTH                           
         SHI   RF,RAUDTXHL         SUBTRACT HEADER LENGTH                       
         BZ    DSPTXTX             NO TEXT TO DISPLAY                           
*                                                                               
         CHI   RF,L'LSCMT          DON'T OVERRUN AVAILABLE SPACE                
         BNH   *+8                                                              
         LHI   RF,L'LSCMT                                                       
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LSCMT(0),RAUDTEXT   DISPLAY TEXT                                 
*                                                                               
DSPTXTX  DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         SELECT FIELD VALIDATED                       
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
*                                                                               
         OI    6(R7),X'80'         TURN ON TRANSMIT BIT                         
*                                  DESCRIPTION FIELD                            
DISPLINX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3,R6                                                            
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - CLRSCRN'                       
***********************************************************************         
*                                                                     *         
*  CLRSCRN:   CLEAR ALL SEL AND DESCRIPTION FIELDS ON LIST SCREEN     *         
*                                                                     *         
***********************************************************************         
*                                                                               
CLRSCRN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,ADLCSELH         SET A(1ST SEL         FIELD)                 
         LA    R3,ADLPFKSH         SET A(EOSCREEN)                              
*                                                                               
         MVI   WORK2,C' '          FIELD OF SPACES                              
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
*                                                                               
CLSLOOP  EQU   *                                                                
*                                                                               
         CR    R2,R3               END OF SCREEN REACHED?                       
         BNL   CLSDONE             YES - EXIT                                   
*                                                                               
         LLC   RE,0(R2)            LENGTH OF SCREEN FIELD                       
*                                                                               
         LR    RF,RE               COPY TOTAL LENGTH                            
*                                                                               
         SHI   RF,8                DECREMENT BY HEADER LENGTH                   
*                                                                               
         TM    1(R2),X'02'         IF EXTENDED FLD HDR?                         
         BZ    *+8                                                              
         SHI    RF,8                  DECREMENT BY TRAILING HDR LEN             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK2       CLEAR FIELD                                  
*                                                                               
         FOUT  (R2)                FORCE RE-DISPLAY                             
*                                                                               
CLSCONT  DS    0H                                                               
         LA    R2,0(RE,R2)         NEXT FIELD                                   
*                                                                               
         B     CLSLOOP             GO BACK FOR NEXT                             
*                                                                               
CLSDONE  EQU   *                                                                
*                                                                               
CLRSCRNX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - CLRAUD'                        
***********************************************************************         
*                                                                     *         
*  CLRAUD:    CLEAR ALL TEXT LINES ON DISPLAY SCREEN                  *         
*                                                                     *         
***********************************************************************         
*                                                                               
CLRAUD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,AUDRCMTH         SET A(1ST SEL         FIELD)                 
         LA    R4,AUDCMTTH         SET A(STATION COOMMENTS HEADER)              
         LA    R3,AUDTABH          SET A(EOCOMMENTS)                            
*                                                                               
         MVI   WORK2,C' '          FIELD OF SPACES                              
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
*                                                                               
CLALOOP  EQU   *                                                                
*                                                                               
         CR    R2,R3               END OF SCREEN REACHED?                       
         BNL   CLADONE             YES - EXIT                                   
*                                                                               
         LLC   RE,0(R2)            LENGTH OF SCREEN FIELD                       
*                                                                               
         CR    R2,R4               SKIP STATION COMMENTS HEADER                 
         BE    CLACONT                                                          
*                                                                               
         LR    RF,RE               COPY TOTAL LENGTH                            
*                                                                               
         SHI   RF,8                DECREMENT BY HEADER LENGTH                   
*                                                                               
         TM    1(R2),X'02'         IF EXTENDED FLD HDR?                         
         BZ    *+8                                                              
         SHI    RF,8                  DECREMENT BY TRAILING HDR LEN             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK2       CLEAR FIELD                                  
*                                                                               
         FOUT  (R2)                FORCE RE-DISPLAY                             
*                                                                               
CLACONT  DS    0H                                                               
         LA    R2,0(RE,R2)         NEXT FIELD                                   
*                                                                               
         B     CLALOOP             GO BACK FOR NEXT                             
*                                                                               
CLADONE  EQU   *                                                                
*                                                                               
CLRAUDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - UNPROT'                        
***********************************************************************         
*                                                                     *         
*  UNPROT:    UNPROTECT TEXT FIELDS FOR FOUT.  ALSO UNPROTECT ALL     *         
*        'SELECT' FIELDS IN EVENT ANY WERE 'PROTECTED'                *         
*                                                                     *         
***********************************************************************         
*                                                                               
UNPROT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,ADLCDESH         SET A(1ST DESCRIPTION FIELD)                 
         LA    R3,ADLLAST          SET A(EOSCREEN)                              
*                                                                               
UNPR0020 EQU   *                                                                
*                                                                               
         CR    R2,R3               END OF SCREEN REACHED?                       
         BNL   UNPR0080            YES - EXIT                                   
*                                                                               
         NI    1(R2),X'FF'-X'20'   TURN OFF PROTECT BIT (DESCRIPT)              
         LA    R2,NEXTSELF(R2)     BUMP TO NEXT LINE                            
*                                                                               
         B     UNPR0020            GO BACK FOR NEXT                             
*                                                                               
UNPR0080 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - PROT'                          
***********************************************************************         
*                                                                     *         
*  PROT:    PROTECT TEXT FIELDS AFTER FOUT.  PROTECT SELECT FIELDS    *         
*        ALSO.  WILL BE OPENED UP WHEN SELECTION DATA PUT IN.         *         
*                                                                     *         
***********************************************************************         
*                                                                               
PROT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R7,ADLCSELH         SET A(1ST SELECT FIELD)                      
         LA    R2,ADLCDESH         SET A(1ST DESCRIPTION FIELD)                 
         LA    R3,ADLLAST          SET A(EOSCREEN)                              
*                                                                               
PROT0020 EQU   *                                                                
*                                                                               
         CR    R2,R3               END OF SCREEN REACHED?                       
         BNL   PROT0080            YES - EXIT                                   
*                                                                               
         OI    1(R2),X'08'         TURN ON  HIGH INTENSITY (DESCRIPT)           
         OI    1(R2),X'20'         TURN ON  PROTECT BIT (DESCRIPT)              
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
         OI    1(R7),X'20'         TURN ON  PROTECT BIT (SELECT)                
         OI    6(R7),X'80'         TURN ON TRANSMIT BIT                         
*                                                                               
         LA    R2,NEXTSELF(R2)     BUMP TO NEXT LINE                            
         LA    R7,NEXTSELF(R7)     BUMP TO NEXT LINE                            
*                                                                               
         B     PROT0020            GO BACK FOR NEXT                             
*                                                                               
PROT0080 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - PR'                            
*******************************************************************             
*                                                                 *             
*        PRINT REPORT                                             *             
*                                                                 *             
*******************************************************************             
*                                                                               
         DS    0D                                                               
PR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        INITIALIZE REPORT AND OPEN PQ                                          
*                                                                               
         BRAS  RE,PRINIT                                                        
*                                                                               
         L     R7,ASPOOLD          ESTABLISH SPOOL WORKAREA                     
         USING SPOOLD,R7                                                        
*                                                                               
*        READ AUDIT RECORDS FOR REPORT                                          
*                                                                               
         XC    KEY,KEY             INIT STARTING KEY                            
*                                                                               
         MVI   RAUDKTYP,X'4D'      SET RECORD CODE                              
         MVC   RAUDKREP,RCONKREP   SET REP CODE                                 
         MVC   RAUDKCON,RCONKCON   SET CONTRACT NUMBER                          
*                                                                               
         MVC   KEY(L'RAUDKEY),RAUDREC SET KEY FOR READ                          
*                                                                               
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH               READ FIRST AUDIT COMMENT RECORD              
*                                                                               
PRLP     DS    0H                                                               
*                                                                               
         CLC   KEY(RAUDKVER-RAUDKEY),KEYSAVE    DONE IF NEW CONTRACT            
         BNE   PRDN                                                             
*                                                                               
         CLI   KEY+19,0            SKIP IF OLD KEY                              
         BE    PRCN                                                             
*                                                                               
         CLI   KEY+26,0            SKIP IF OLD KEY                              
         BE    PRCN                                                             
*                                                                               
         GOTO1 VGETREC,DMCB,RAUDREC READ IN AUDIT COMMENT RECORD                
*                                                                               
         CLC   =C'AUL ',CONACT     SKIP IF FULL LIST                            
         BE    PRFILTX                                                          
*                                                                               
*        FILTER ON REP/STA                                                      
*                                                                               
         MVI   ELCODE,RAUDTHQ      SET HEADER ELEMENT CODE                      
         LA    R6,RAUDREC          SEARCH AUDIT COMMENT RECORD                  
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND ELEMENT                            
*                                                                               
         USING RAUDTHEL,R6         ESTABLISH HEADER ELEMENT                     
*                                                                               
         CLC   =C'AULR',CONACT     IF FILTERING ON REP COMMENTS                 
         BNE   PRREPX                                                           
*                                                                               
         CLI   RAUDTHTP,C'R'       DROP RECORD IF NOT FOR A REP                 
         BNE   PRCN                                                             
*                                                                               
         B     PRFD                ELSE KEEP                                    
*                                                                               
PRREPX   DS    0H                                                               
*                                                                               
         CLC   =C'AULS',CONACT     IF FILTERING ON STATION COMMENTS             
         BNE   PRSTAX                                                           
*                                                                               
         CLI   RAUDTHTP,C'S'       DROP REOCORD IF NOT FOR A STATION            
         BNE   PRCN                                                             
*                                                                               
         B     PRFD                ELSE KEEP                                    
*                                                                               
PRSTAX   DS    0H                                                               
*                                                                               
PRFILTX  DS    0H                                                               
*                                                                               
PRFD     DS    0H                  LIST RECORD ON SCREEN                        
*                                                                               
         BRAS  RE,PRLINE           PRINT AUDIT COMMENTS                         
*                                                                               
PRCN     DS    0H                                                               
*                                                                               
         GOTO1 VSEQ                READ NEXT AUDIT COMMENT KEY                  
*                                                                               
         B     PRLP                                                             
*                                                                               
PRDN     DS    0H                                                               
*                                                                               
         BRAS  RE,PRCLOSE          CLOSE PRINT QUEUE                            
*                                                                               
PRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - PRINIT'                        
*******************************************************************             
*                                                                 *             
*        PRINT REPORT - INITIALIZATION                            *             
*                                                                 *             
*******************************************************************             
*                                                                               
         DS    0D                                                               
PRINIT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A0C',0                                      
         MVC   SPOOL,0(R1)      A(SPOOL)                                        
*                                                                               
         L     R7,ASPULAR          ESTABLISH SPOOL CONTROL BLOCK                
         USING SPOOLD,R7                                                        
*                                                                               
         ST    R7,ASPOOLD                                                       
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
*                                                                               
         L     R1,AFACILS                                                       
         LM    R2,R4,8(R1)                                                      
         ST    R3,ATIA                                                          
*                                                                               
         L     RF,=V(SQUASHER)                                                  
         A     RF,TWAREL4D         RELOCATE ADDRESS                             
         ST    RF,SQUASHER     A(SQUASHER)                                      
*                                                                               
         MVC   SPOOLDM,DATAMGR                                                  
         MVC   SPOOLBUF,ATIA                                                    
         XC    VPRINT,VPRINT                                                    
         MVC   RCDATCON,DATCON                                                  
         MVC   RCCOMFAC,ACOMFACS                                                
*                                                                               
         XC    TWAAGNM2,TWAAGNM2   INIT AGENCY NAME                             
*                                                                               
*          OPEN PRINT QUEUE                                                     
*                                                                               
PRPQOPEN XC    SPOOLKEY,SPOOLKEY                                                
*                                                                               
         OI    SPOOLIND,X'40'   ALLINIT SPOOL KEY-CLASS,LPP,COPIES              
*                                                                               
         LA    R3,SPOOLKEY 6                                                    
         MVC   SPOOLKEY+12(3),=C'AUD'  REPORT ID                                
*                                                                               
         MVC   SPOOLKEY+1(11),SPACES                                            
         MVC   1(8,R3),CONCNUM                                                  
*                                                                               
         CLI   1(R3),C'0'                                                       
         BNE   *+20                                                             
         MVC   1(7,R3),2(R3)                                                    
         XC    8(1,R3),8(R3)       CLEAN UP TRAILING DIGIT                      
         B     *-20                                                             
*                                                                               
         GOTO1 SQUASHER,DMCB,SPOOLKEY+1,11                                      
*                                                                               
         MVI   SPOOLKEY+16,60      60 LINES TO A PAGE                           
*                                                                               
         MVI   SPMODE,0                                                         
*                                                                               
         LA    RF,SPOOLKEY                                                      
         USING PQPLD,RF                                                         
*                                                                               
         MVI   PLCLASS,C'K'        CLASS K                                      
*                                                                               
         DROP  RF                                                               
*                                                                               
         LA    RE,TWASPKEY                                                      
*                                                                               
         ST    RE,SPOOLQLK         SET EXTENDED KEY ADDRESS                     
         USING PQPLD,RE                                                         
*                                                                               
         XC    0(133,RE),0(RE)                                                  
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLRETNL,=H'6'                                                    
         MVC   QLRETND,=H'2'                                                    
*                                                                               
         DROP  RE                                                               
*                                                                               
         GOTO1 SPOOL,PARAS,SPOOLD  OPEN PRINT QUEUE                             
*                                                                               
         MVC   SPOOLRPN,SPOOLKEY+19                                             
*                                                                               
*        SET HEADLINE SPECS AND ROUTINE                                         
*                                                                               
         LA    RF,PRHOOK                                                        
         ST    RF,HEADHOOK                                                      
         LA    RF,HEDSPECS                                                      
         ST    RF,SPECS                                                         
*                                                                               
PRINITX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - PRCLOSE'                       
***********************************************************************         
*                                                                     *         
*   PRCLOSE:   CLOSE PRINT QUEUE                                      *         
*                                                                     *         
*NTR     RAUDREC - CONTAINS RECORD FOR DISPLAY                        *         
*                                                                     *         
***********************************************************************         
*                                                                               
         DS    0D                  ALIGNMENT                                    
PRCLOSE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        END OF REPORT                                                          
*                                                                               
*        CLOSE REPORT ON PQ AND SEND CONFIRMATION MSG                           
*                                                                               
         MVC   INTREPNO,SPOOLRPN                                                
         MVI   SPMODE,X'FF'                                                     
         GOTO1 SPOOL,PARAS,SPOOLD  CLOSE PRINT QUEUE                            
*                                                                               
         MVC   IOAREA(48),OKMESS   BASIC CONFIRMATION MESSAGE                   
         MVC   IOAREA(3),SPOOLKEY+12   REPORT ID                                
*                                                                               
         LA    R1,IOAREA                                                        
*                                                                               
         EDIT  (2,SPOOLRPN),(5,4(R1)),ALIGN=LEFT   REPORT NUMBER                
         EDIT  (2,SPOOLPAG),(3,34(R1)),ALIGN=LEFT  REPORT PAGES                 
         EDIT  (2,SPOOLLIN),(4,44(R1)),ALIGN=LEFT  REPORT LINES                 
*                                                                               
         GOTO1 SQUASHER,DMCB,IOAREA,48                                          
*                                                                               
         OI    CONMSGH+6,X'80'     XMIT MESSAGE                                 
         XC    CONMSG,CONMSG                                                    
         MVC   CONMSG(48),IOAREA                                                
*                                                                               
PRCLOSEX DS    0H                                                               
         XIT1                                                                   
*                                                                               
OKMESS   DC    C'XXX,12345 HAS BEEN SPOOLED. PAGES=NNN,LINES=NNNN'              
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - PRLINE'                        
***********************************************************************         
*                                                                     *         
*   PRLINEN:   PRINT   AUDIT COMMENT RECORD IN LIST                   *         
*                                                                     *         
*NTR     RAUDREC - CONTAINS RECORD FOR DISPLAY                        *         
*                                                                     *         
***********************************************************************         
*                                                                               
         DS    0D                  ALIGNMENT                                    
PRLINE   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   PRKEY,KEY           SAVE CURRENT KEY                             
*                                                                               
*        FIND HEADER ELEMENT                                                    
*                                                                               
         MVI   ELCODE,RAUDTHQ      SET HEADER ELEMENT CODE                      
         LA    R6,RAUDREC          SEARCH AUDIT COMMENT RECORD                  
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND ELEMENT                            
*                                                                               
         USING RAUDTHEL,R6         ESTABLISH HEADER ELEMENT                     
*                                                                               
         MVC   WORK3,RAUDTHEL      SAVE HEADER ELEMENT                          
*                                                                               
         LA    R3,P                POINT TO PRINT LINE                          
*                                                                               
         USING PRLISTD,R3          ESTABLISH PRINT LINES                        
*                                                                               
         MVC   P,SPACES            INIT PRINT LINE                              
*                                                                               
         MVC   PL1TYP,RAUDTHTP     DISPLAY SENDER TYPE                          
*                                                                               
*        MOD/VERSION NUMBER LEFT JUSTIFIED                                      
*                                                                               
         LA    R4,PL1VER           VERSION PRINT AREA                           
*                                                                               
         CLI   RAUDKMOD,X'FF'      SKIP IF NO MOD NUMBER                        
         BE    PLNMODX                                                          
*                                                                               
         MVC   FULL+1(1),RAUDKMOD  COMPLEMENTED MOD NUMBER                      
         XI    FULL+1,X'FF'        UNCOMPLEMENT                                 
*                                                                               
         LLC   RF,FULL+1           GET MOD NUMBER                               
         SHI   RF,1                DECREMENT BY ONE                             
         CVD   RF,DUB                                                           
*                                                                               
         MVC   FULL,=X'40202120'   EDIT PATTERN                                 
         ED    FULL(4),DUB+6                                                    
*                                                                               
         MVI   0(R4),C'M'                                                       
         LHI   RF,2                MAX MOD DIGITS                               
         LA    R1,FULL+2                                                        
*                                                                               
         CLI   0(R1),C' '          LEFT JUSTIFY                                 
         BH    *+12                                                             
         SHI   RF,1                                                             
         AHI   R1,1                                                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),0(R1)                                                    
*                                                                               
         LA    R4,2(RF,R4)         NEXT PRINT AREA                              
*                                                                               
         MVI   0(R4),C'/'          SEPARATOR                                    
         MVI   1(R4),C'V'          VERSION ID                                   
         LA    R4,2(R4)            NEXT PRINT AREA                              
*                                                                               
PLNMODX  DS    0H                                                               
*                                                                               
         MVC   FULL(1),RAUDKVER    COMPLEMENTED VERSION NUMBER                  
         XI    FULL,X'FF'          UMCOMPLEMENT VERSION NUMBER                  
*                                                                               
         LLC   RF,FULL                                                          
         CVD   RF,DUB              CVD                                          
*                                                                               
         MVC   FULL,=X'40202020'   EDIT PATTERN                                 
         ED    FULL(4),DUB+6                                                    
*                                                                               
         LHI   RF,3                MAX VERSION DIGITS                           
         LA    R1,FULL+1                                                        
*                                                                               
         CLI   0(R1),C' '          LEFT JUSTIFY                                 
         BH    *+12                                                             
         AHI   R1,1                                                             
         BCT   RF,*-12                                                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R1)                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(2,RAUDTHDT),(17,PL1DATE)   SENDING DATE             
*                                                                               
         MVC   PL1TIME(2),RAUDTHTM    DISPLAY TIME                              
         MVI   PL1TIME+2,C':'                                                   
         MVC   PL1TIME+3(2),RAUDTHTM+2                                          
*                                                                               
*        PID DISPLAY                                                            
*                                                                               
         SR    R2,R2               NO SCREEN FIELD INVOLVED                     
*                                                                               
         GOTOR TRNPID,DMCB,(R6),(L'PL1PID,PL1PID)   TRANSLATE PID               
*                                                                               
*        SALES ASSISTANT DISPLAY                                                
*                                                                               
         CLI   RAUDTHLN,RAUDTHL    SKIP IF SHORT ELEMENT                        
         BL    *+10                                                             
         MVC   PL1ASST,RAUDTHAS        SALES ASSISTANT                          
*                                                                               
*        CONTRACT TOTAL                                                         
*                                                                               
         EDIT  (P8,RAUDTH$),PL1TOT$,2,COMMAS=YES,FLOAT=-                        
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD,0 PRINT LINE                                   
*                                                                               
*        DISPLAY COMMENT LINES                                                  
*                                                                               
         MVC   P,SPACES            INIT PRINT AREA                              
*                                                                               
         MVC   PL2TTL,=CL9'REP CMT'   SET TITLE                                 
*                                                                               
         DROP  R6                                                               
*                                                                               
         LA    R5,WORK3            ESTABLISH SAVED HEADER ELEMENT               
         USING RAUDTHEL,R5                                                      
*                                                                               
         CLI   RAUDTHTP,C'S'       IF STATION TYPE VERSION                      
         BNE   *+10                                                             
         MVC   PL2TTL,=CL9'STA CMT'   SET TITLE                                 
*                                                                               
         LA    R6,RAUDREC          FIND AUDIT COMMENTS                          
         MVI   ELCODE,RAUDTXQ      ELEMENT ID                                   
         BRAS  RE,GETEL            FIND FIRST                                   
         BNE   PLACMDN             NONE FOUND                                   
*                                                                               
PLACMLP  DS    0H                                                               
*                                                                               
         BNE   PLACMDN             NONE FOUND                                   
*                                                                               
         USING RAUDTXEL,R6         ESTABLISH AUDIT TEXT ELEMENT                 
*                                                                               
         LLC   RF,RAUDTXLN         ELEMENT LENGTH                               
         SHI   RF,RAUDTXHL         DECREMENT BY ELM HEADER LENGTH               
         BZ    PLACMCN             SKIP IF NO COMMENT                           
*                                                                               
         CHI   RF,L'PL2COM         MAKE SURE IT FITS ON SCREEN                  
         BNH   *+8                                                              
         LHI   RF,L'PL2COM                                                      
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PL2COM(0),RAUDTEXT    PRINT COMMENT                              
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD,0   PRINT LINE                                 
*                                                                               
         MVC   P,SPACES                                                         
*                                                                               
PLACMCN  DS    0H                                                               
*                                                                               
         LLC   RF,0(R2)            GET FIELD LENGTH                             
         LA    R2,0(RF,R2)         NEXT FIELD                                   
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT COMMENT ELEMENT                    
*                                                                               
         B     PLACMLP             PROCESS NEXT ELEMENT                         
*                                                                               
PLACMDN  DS    0H                                                               
*                                                                               
         MVC   P,SPACES            PRINT BLANK LINE                             
         GOTO1 SPOOL,DMCB,SPOOLD,0                                              
         MVC   P,SPACES                                                         
*                                                                               
*        BUYER'S NOTES                                                          
*                                                                               
         CLI   RAUDTHTP,C'S'       SKIP IF STATION TYPE VERSION                 
         BE    *+10                                                             
         MVC   PL2TTL,=CL7'NOTES'     TITLE FOR COMMENTS                        
*                                                                               
         LA    R6,RAUDREC          FIND AUDIT COMMENTS                          
         MVI   ELCODE,RAUDCMQ      ELEMENT ID                                   
         BRAS  RE,GETEL            FIND FIRST                                   
*                                                                               
PLBCM2LP DS    0H                                                               
*                                                                               
         BNE   PLBCM2DN            NONE FOUND                                   
*                                                                               
         USING RAUDCMEL,R6         ESTABLISH AUDIT TEXT ELEMENT                 
*                                                                               
         LLC   RF,RAUDCMLN         ELEMENT LENGTH                               
         SHI   RF,RAUDCMHL         DECREMENT BY ELM HEADER LENGTH               
         BZ    PLBCM2CN            SKIP IF NO COMMENT                           
*                                                                               
         CHI   RF,L'PL2COM         MAKE SURE IT FITS ON SCREEN                  
         BNH   *+8                                                              
         LHI   RF,L'PL2COM                                                      
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PL2COM(0),RAUDCMNT    DISPLAY COMMENT                            
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD,0   PRINT LINE                                 
*                                                                               
         MVC   P,SPACES                                                         
*                                                                               
PLBCM2CN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT COMMENT ELEMENT                    
*                                                                               
         B     PLBCM2LP            PROCESS NEXT ELEMENT                         
*                                                                               
PLBCM2DN DS    0H                                                               
*                                                                               
         MVC   PL2TTL,=CL7'     '     CLEAR TITLE IN CASE NO NOTES              
         GOTO1 SPOOL,DMCB,SPOOLD,0 PRINT SPACING LINE                           
*                                                                               
PLBCMX   DS    0H                                                               
*                                                                               
PRLINEX  DS    0H                                                               
*                                                                               
         MVC   KEY,PRKEY           RESTORE CURRENT KEY                          
         GOTO1 VHIGH               RESTORE FILE POINTERS                        
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3,R5,R6                                                         
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - PRHOOK'                        
*******************************************************************             
*                                                                 *             
*        PRINT REPORT - HEDALINE HOOK ROUTINE                     *             
*                                                                 *             
*******************************************************************             
*                                                                               
         DS    0D                                                               
PRHOOK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        FILL IN VARIABLE PARTS OF THE HEADLINES                                
*                                                                               
*              PAGE NUMBER                                                      
*                                                                               
PRPAGE   DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,PAGE           GET PAGE NUMBER                              
*                                                                               
         CVD   RF,DUB                                                           
         MVC   H1+121(4),=X'40202120' EDIT PATTERN                              
         ED    H1+121(4),DUB+6     DISPLAY PAGE NUMBER                          
*                                                                               
PRPAGEX  DS    0H                                                               
*                                                                               
*              REP NAME                                                         
*                                                                               
         MVC   H1(33),TWAREPNM     REP NAME                                     
*                                                                               
*        RUN DATE                                                               
*                                                                               
PRRUN    DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(25,WORK)  RUN TODAY/TIME                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(8,H2+107)  DATE                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WORK+3           HOURS                                        
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  FULL(3),DUB+6(2)                                                 
         MVC   H2+113(2),FULL+1                                                 
*                                                                               
         MVI   H2+115,C':'                                                      
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WORK+4           MINUTES                                      
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  FULL(3),DUB+6(2)                                                 
         MVC   H2+116(2),FULL+1                                                 
*                                                                               
PRRUNX   DS    0H                                                               
*                                                                               
*              STATION                                                          
*                                                                               
PRSTN    XC    IOAREA(32),IOAREA   BUILD STATION KEY                            
*                                                                               
         MVI   RSTAKTYP,2                                                       
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RCONKSTA                                                
         MVC   RSTAMKT,WSTAEXP                                                  
*                                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 VREAD                                                            
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         MVC   WORK(4),RSTAKSTA    STATION CALL LETTERS                         
*                                                                               
         MVC   WORK+4(3),=C'-FM'   FILL IN BROADCAST BAND                       
*                                                                               
         CLI   RSTAKSTA+4,C'F'                                                  
         BE    PRSTN10                                                          
*                                                                               
         MVI   WORK+5,C'A'                                                      
         CLI   RSTAKSTA+4,C'A'                                                  
         BE    PRSTN10                                                          
*                                                                               
         MVI   WORK+5,C'C'                                                      
         CLI   RSTAKSTA+4,C'C'                                                  
         BE    PRSTN10                                                          
*                                                                               
         MVC   WORK+5(2),=C'L '                                                 
         CLI   RSTAKSTA+4,C'L'                                                  
         BE    PRSTN10                                                          
*                                                                               
         MVC   WORK+5(2),=C'TV'                                                 
*                                                                               
PRSTN10  CLI   WORK+3,C' '         ADJUST FOR 3 LETTER STATION CODE             
         BNE   *+14                                                             
         MVC   WORK+3(3),WORK+4                                                 
         MVI   WORK+6,C' '                                                      
*                                                                               
         MVC   H3+13(7),WORK       STATION                                      
*                                                                               
         MVC   H3+23(L'RSTAMKT),RSTAMKT - MARKET NAME                           
*                                                                               
PRSTNX   DS    0H                                                               
*                                                                               
*        CONTRACT NUMBER                                                        
*                                                                               
         MVC   H3+66(8),CONCNUM    CONTRACT NUMBER                              
*                                                                               
*        AGENCY NAME                                                            
*                                                                               
         XC    IOAREA(32),IOAREA   INIT KEY                                     
*                                                                               
         MVI   RAGYKTYP,X'0A'      AGENCY REC TYPE                              
         MVC   RAGYKAGY,RCONKAGY   AGY                                          
         MVC   RAGYKAOF,RCONKAOF   OFFICE                                       
         MVC   RAGYKREP,REPALPHA   REP                                          
*                                                                               
         MVC   KEY,IOAREA          SET KEY                                      
         MVC   RAGYNAM1,WAGYEXP    INIT AGENCY NAME                             
*                                                                               
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH               READ FOR AGENCY KEY                          
*                                                                               
         CLC   KEY(27),KEYSAVE     SKIP IF NOT FOUND                            
         BNE   PRAGYX                                                           
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA  READ IN AGENCY RECORD                       
*                                  SAVE AGENCY DATA                             
         MVC   TWAAGNM1,RAGYNAM1   SCREEN NAME                                  
         MVC   TWAAGNM2,RAGYNAM2   CONTRACT NAME                                
         MVC   TWAAGSTT,RAGYSTAT   STATE                                        
         MVC   TWAAGZIP,RAGYZIP    ZIp CODE                                     
         MVC   TWAAEASY,RAGYPRO2   PROFILE FOR EASYLINK AGY COPY                
         MVC   TWAARISK,RAGYRISK   AGENCY CREDIT RISK                           
         MVC   TWAALIAB,RAGYLIAB   AGENCY LIABILITY POSITION                    
*                                                                               
         MVC   H5+13(4),RAGYKAGY  DISPLAY AGENCY                                
*                                                                               
         LAY   R1,MYSPACES                                                      
         CLC   RAGYKAOF,0(R1)      OFFICE?                                      
         BE    PRAGOFCX                                                         
*                                                                               
         LA    RE,H5+13            YES - DISPLAY OFFICE                         
         MVI   H5+13+4,C' '                                                     
*                                                                               
         CLI   0(RE),C' '          FIND FIRST BLANK IN AGENCY FIELD             
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
*                                                                               
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),RAGYKAOF    AGENCY OFFICE                                
*                                                                               
PRAGOFCX DS    0H                                                               
*                                                                               
         MVC   H5+23(33),RAGYNAM2  AGENCY NAME                                  
*                                                                               
PRAGYX   DS    0H                                                               
*                                                                               
*              ADVERTISER                                                       
*                                                                               
         XC    IOAREA(32),IOAREA   READ INTO IOAREA                             
*                                                                               
         MVI   RADVKTYP,8          ADVRT RECORD CODE                            
         MVC   RADVKADV,RCONKADV   ADVERTISER                                   
         MVC   RADVKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         MVC   RADVNAME,WADVEXP    PRE SET ADVERTISER NAME                      
*                                                                               
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PRADVX                                                           
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         MVC   H5+102(4),RADVKADV  DISPLAY ADVERTISER CODE                      
         MVC   H5+107(L'RADVNAME),RADVNAME    NAME                              
*                                                                               
PRADVX   DS    0H                                                               
*                                                                               
*              SALESPERSON                                                      
*                                                                               
PRSLS    XC    IOAREA(32),IOAREA                                                
*                                                                               
         MVI   RSALKTYP,6          KEY TYPE - SALESMAN RECORD                   
         MVC   RSALKREP,REPALPHA   REP                                          
         MVC   RSALKSAL,RCONSAL    SALESMAN CODE                                
         MVC   KEY,IOAREA                                                       
         MVC   RSALNAME,WSALEXP                                                 
*                                                                               
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH               READ IN SALESPERSON RECORD                   
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PRSLSX                                                           
*                                                                               
*                                                                               
         MVC   H6+13(3),RSALKSAL  SALESMAN CODE                                 
         MVC   H6+23(L'RSALNAME),RSALNAME                                       
*                                                                               
PRSLSX   DS    0H                                                               
*                                                                               
*        PRODUCT                                                                
*                                                                               
PRPRD    DS    0H                                                               
*                                                                               
*        GET PRODUCT RECORD                                                     
*                                                                               
         CLC   =C'   ',RCONPRD     IF PRODUCT IS SPACES                         
         BL    PRPRD05                                                          
*                                                                               
         LA    R6,RCONREC             FIND PRODUCT NAME ELEMENT                 
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                   NO X'05'                                  
*                                                                               
         MVC   H6+107(20),2(R6)       PRODUCT NAME                              
*                                                                               
         B     PRPRDX                                                           
*                                                                               
PRPRD05  DS    0H                                                               
*                                                                               
         XC    IOAREA(32),IOAREA                                                
*                                                                               
         MVI   RPRDKTYP,9                                                       
         MVC   RPRDKADV,RCONKADV                                                
         MVC   RPRDKPRD,RCONPRD                                                 
         MVC   RPRDKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PRPRDX                                                           
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         MVC   H6+102(3),RCONPRD                                                
         MVC   H6+107(20),RPRDNAME                                              
*                                                                               
         B     PRPRDX                                                           
*                                                                               
*              FIND PRODUCT ELEMENT                                             
*                                                                               
PRPRD10  DS    0H                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO X'05'                                     
*                                                                               
         MVC   H6+99(3),2(R6)      PRODUCT EXPANSION                            
*                                                                               
PRPRDX   DS    0H                                                               
*                                                                               
*        CONTRACT DATES                                                         
*                                                                               
PRCDT    DS    0H                                                               
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,(X'80',RCONREC),H7+13,0,DUB               
*                                                                               
PRCDTX   DS    0H                                                               
*                                                                               
         CR    RB,RB               FORCE EQ CC                                  
*                                                                               
PRHOOKX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - HEDSPECS'                      
*******************************************************************             
*                                                                 *             
*        HEADLINE SPECS FOR AUDIT REPORTS                         *             
*                                                                 *             
*******************************************************************             
*                                                                               
HEDSPECS DS    0D                                                               
         SPROG                                                                  
         PSPEC H1,60,C'AUDIT REPORT'                                            
         PSPEC H2,60,C'------------'                                            
         PSPEC H1,117,C'PAGE'                                                   
         PSPEC H2,100,C'RUN ON'                                                 
         PSPEC H2,110,C'AT'                                                     
         PSPEC H3,1,C'STATION    :'                                             
         PSPEC H3,57,C'CONTRACT:'                                               
         PSPEC H5,1,C'AGENCY     :'                                             
         PSPEC H5,90,C'ADVERTISER:'                                             
         PSPEC H6,1,C'SALESPERSON:'                                             
         PSPEC H6,90,C'PRODUCT   :'                                             
         PSPEC H7,1,C'FLIGHT     :'                                             
         PSPEC H8,132,C'-'                                                      
*                                                                               
         PSPEC H9,1,C'VER'                                                      
         PSPEC H10,1,C'---'                                                     
         PSPEC H9,11,C'CRD DATE/TIME'                                           
         PSPEC H10,11,C'-------------'                                          
         PSPEC H9,30,C'PID'                                                     
         PSPEC H10,30,C'---'                                                    
         PSPEC H9,70,C'ASST'                                                    
         PSPEC H10,70,C'----'                                                   
         PSPEC H9,100,C'TOTAL DOLLARS'                                          
         PSPEC H10,100,C'-------------'                                         
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTBED                                                       
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTBFD                                                       
*                                                                               
         ORG   LOCALTWA                                                         
TWAREL4D DS    A                   RELOCATION FACTOR                            
PRKEY    DS    CL32                CURRENT KEY SAVEAREA                         
SVREPDA  DS    XL4                 DA OF LATEST REP AUDIT REC                   
SVCONNUM DS    XL(L'RCONKCON)      CONTRACT NUMBER SAVEAREA                     
UPDOK    DS    XL1                 X'FF' - OKAY TO UPDATE                       
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - LISTD'                         
***********************************************************************         
*                                                                     *         
*        LISTD - DSECT FOR LIST SCREEN DESCRIPTION OF AUDIT RECORD    *         
*                                                                     *         
***********************************************************************         
*                                                                               
LISTD    DSECT                     LIST DESCRIPTION                             
LSLINE   DS    0X                                                               
LSTYP    DS    CL1                 COMMENTS SENDER TYPE - R/S                   
LSVER    DS    CL8                 VERSION NUMBER                               
         DS    CL1                 SPARE                                        
LSDATE   DS    CL8                 DATE                                         
         DS    CL2                 SPARE                                        
LSCMT    DS    CL52                COMMENT                                      
LSLISTL  EQU   *-LISTD             LENGTH OF LINE DISPLAY                       
*                                                                               
         TITLE 'T8024D - REPPAK AUDIT COMMENTS - PRLISTD'                       
***********************************************************************         
*                                                                     *         
*        PRLISTD - DSECT FOR PRINT LINE DESCRIPTION OF AUDIT RECORD   *         
*                                                                     *         
***********************************************************************         
*                                                                               
PRLISTD  DSECT                     PRINT LINES DESCRIPTION                      
PL1LINE  DS    0X                                                               
PL1TYP   DS    CL1                 COMMENTS SENDER TYPE - R/S                   
PL1VER   DS    CL8                 VERSION NUMBER                               
         DS    CL1                 SPARE                                        
PL1DATE  DS    CL8                 DATE                                         
         DS    CL2                 SPARE                                        
PL1TIME  DS    CL7                 TIME                                         
         DS    CL2                 SPARE                                        
PL1PID   DS    CL30                PID                                          
         DS    CL10                SPARE                                        
PL1ASST  DS    CL6                 BUYER'S ASSISTANT                            
         DS    CL22                SPARE                                        
PL1TOT$  DS    CL15                TOTAL DOLLARS                                
         DS    CL(132-(*-PRLISTD)) SPARE                                        
PL1LISTL EQU   *-PRLISTD             LENGTH OF LINE DISPLAY                     
*                                                                               
         ORG   PL1LINE                                                          
PL2LINE  DS    0X                                                               
         DS    CL10                SPARE                                        
PL2TTL   DS    CL9                 TITLE FOR COMMENTS                           
         DS    CL10                SPARE                                        
PL2COM   DS    CL70                COMMENT                                      
         DS    CL(132-(*-PRLISTD)) SPARE                                        
PL2LISTL EQU   *-PRLISTD             LENGTH OF LINE DISPLAY                     
*                                                                               
       ++INCLUDE CTGENFILE                                                      
*                                                                               
       ++INCLUDE FATIOB                                                         
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
*                                                                               
       ++INCLUDE DMPRTQL                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036RECNT4D   05/16/12'                                      
         END                                                                    
